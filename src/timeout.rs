//! A deadline the match loop can observe.
//!
//! Catastrophic backtracking happens inside a single `onig_search()`
//! call, so nothing outside the engine can interrupt it: splitting the
//! search does not help, and a watchdog thread has no safepoint to
//! signal. The only point inside the loop is the three
//! `CHECK_INTERRUPT_IN_MATCH_AT` sites in `regexec.c`, which CRuby's
//! build uses for `rb_thread_check_ints()` and which a standalone build
//! compiled away entirely.
//!
//! The vendored Onigmo now calls an embedder hook there instead
//! (`onig_set_interrupt_func`). This module is that hook: it reads a
//! thread-local deadline and, past it, tells the engine to abort the
//! match with `ONIGERR_TIMEOUT`, which surfaces as
//! [`OnigmoError::is_timeout`].
//!
//! The deadline is per thread and scoped, so a caller that sets one for
//! its own match cannot leak it into an unrelated match on the same
//! thread — including a nested one, since the guard restores whatever
//! was in place before it.

use std::cell::Cell;
use std::time::Instant;

thread_local! {
    /// `None` when no deadline is in force, which is the common case and
    /// the one the hook makes cheapest.
    static DEADLINE: Cell<Option<Instant>> = const { Cell::new(None) };
}

/// The hook Onigmo calls from inside the match loop. Non-zero aborts the
/// match.
///
/// Onigmo only reaches this every `ONIG_INTERRUPT_CHECK_INTERVAL`-th
/// backtrack, so the clock read is amortised; with no deadline set it is
/// a thread-local read and a branch.
///
/// This runs on Onigmo's C stack, so it must not unwind. Everything it
/// does is panic-free.
extern "C" fn check_interrupt() -> std::os::raw::c_int {
    DEADLINE.with(|d| match d.get() {
        Some(deadline) if Instant::now() >= deadline => 1,
        _ => 0,
    })
}

pub(crate) fn install_interrupt_hook_once() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| unsafe {
        crate::onig_set_interrupt_func(Some(check_interrupt));
    });
}

/// Restores the enclosing deadline when dropped, so deadlines nest.
#[derive(Debug)]
pub struct DeadlineGuard {
    previous: Option<Instant>,
}

impl Drop for DeadlineGuard {
    fn drop(&mut self) {
        DEADLINE.with(|d| d.set(self.previous));
    }
}

/// Abort any match that is still running on this thread at `deadline`
/// with a timeout error, until the returned guard is dropped.
///
/// `None` suspends the enclosing deadline rather than inheriting it,
/// which is what a caller that deliberately runs an untimed match wants.
///
/// The deadline is only observed *inside* the match loop, so it bounds
/// backtracking, not the whole call: a search that is slow for some
/// other reason (a huge subject scanned linearly) can still overrun it.
/// CRuby's `Regexp.timeout` has the same shape.
#[must_use = "the deadline is only in force while the guard is alive"]
pub fn set_deadline(deadline: Option<Instant>) -> DeadlineGuard {
    install_interrupt_hook_once();
    let previous = DEADLINE.with(|d| d.replace(deadline));
    DeadlineGuard { previous }
}

/// [`set_deadline`] for a duration measured from now. `None` is no
/// timeout.
#[must_use = "the deadline is only in force while the guard is alive"]
pub fn set_timeout(timeout: Option<std::time::Duration>) -> DeadlineGuard {
    set_deadline(timeout.map(|t| Instant::now() + t))
}

/// The deadline in force on this thread, if any.
pub fn deadline() -> Option<Instant> {
    DEADLINE.with(|d| d.get())
}

#[cfg(test)]
mod tests {
    use crate::*;
    use std::time::Duration;

    /// The reason this machinery exists: a pattern that backtracks
    /// exponentially does so inside one `onig_search` call, and without a
    /// deadline that call never returns.
    ///
    /// The pattern has to be one the match cache cannot memoize across,
    /// or there is no runaway left to cut short — `/^(a*)*$/`, the usual
    /// example, now finishes on its own. The back-reference is what keeps
    /// the cache from applying, which `is_linear_time` states outright.
    #[test]
    fn catastrophic_backtracking_stops_at_the_deadline() {
        let re = Regex::new(r"(a+)+\1b").unwrap();
        assert!(
            !re.is_linear_time(),
            "the pattern must be one the match cache cannot bound, or it \
             would never run away in the first place"
        );
        let subject = "a".repeat(40) + "c";
        let started = std::time::Instant::now();
        let _guard = set_timeout(Some(Duration::from_millis(50)));
        let err = re
            .search(&subject, 0, subject.len(), None)
            .expect_err("the match must be cut short, not run to completion");
        assert!(err.is_timeout(), "expected a timeout, got {err:?}");
        assert_eq!(err.message(), "regexp match timeout");
        assert!(
            started.elapsed() < Duration::from_secs(10),
            "took {:?}, so the deadline was not observed",
            started.elapsed()
        );
    }

    /// A pattern that finishes well inside the deadline is unaffected,
    /// and reports its match as usual.
    #[test]
    fn a_match_inside_the_deadline_is_untouched() {
        let re = Regex::new(r"\d+").unwrap();
        let _guard = set_timeout(Some(Duration::from_secs(30)));
        assert_eq!(re.search("abc 123", 0, 7, None).unwrap(), Some(4));
    }

    /// With no deadline in force the hook never aborts anything — which
    /// is every caller that does not ask for a timeout.
    #[test]
    fn no_deadline_means_no_timeout() {
        let re = Regex::new(r"\d+").unwrap();
        assert!(deadline().is_none());
        assert_eq!(re.search("abc 123", 0, 7, None).unwrap(), Some(4));
    }

    /// The guard restores what it found, so a deadline cannot leak out
    /// of the scope that set it, and a nested one does not clobber the
    /// outer.
    #[test]
    fn deadlines_nest_and_do_not_leak() {
        assert!(deadline().is_none());
        {
            let _outer = set_timeout(Some(Duration::from_secs(60)));
            let outer = deadline().expect("outer deadline");
            {
                let _inner = set_timeout(Some(Duration::from_secs(1)));
                assert!(deadline().expect("inner deadline") < outer);
                {
                    // `None` suspends rather than inherits.
                    let _untimed = set_timeout(None);
                    assert!(deadline().is_none());
                }
                assert!(deadline().expect("inner restored") < outer);
            }
            assert_eq!(deadline(), Some(outer));
        }
        assert!(deadline().is_none());
    }
}
