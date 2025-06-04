use crate::*;

#[repr(transparent)]
pub struct Region(*mut OnigRegion);

impl std::fmt::Debug for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Region( ")?;
        for i in 0..self.len() {
            match self.pos(i) {
                Some((beg, end)) => write!(f, "[{}..{}] ", beg, end)?,
                None => write!(f, "[] ")?,
            }
        }
        write!(f, ")")
    }
}

impl Drop for Region {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe { onig_region_free(self.0, 1) };
        }
    }
}

impl Region {
    pub(crate) fn new() -> Self {
        let region = unsafe { onig_region_new() };
        Self(region)
    }

    pub(crate) fn raw(&self) -> *mut OnigRegion {
        self.0
    }

    pub fn len(&self) -> usize {
        unsafe { (*self.0).num_regs as usize }
    }

    fn beg(&self, i: usize) -> Option<usize> {
        unsafe {
            let idx = *(*self.0).beg.add(i);
            if idx == ONIG_REGION_NOTPOS as _ {
                None
            } else {
                Some(idx as usize)
            }
        }
    }

    fn end(&self, i: usize) -> Option<usize> {
        unsafe {
            let idx = *(*self.0).end.add(i);
            if idx == ONIG_REGION_NOTPOS as _ {
                None
            } else {
                Some(idx as usize)
            }
        }
    }

    pub fn pos(&self, i: usize) -> Option<(usize, usize)> {
        if i < self.len() {
            if let Some(beg) = self.beg(i) {
                return Some((beg, self.end(i).unwrap()));
            }
        }
        None
    }

    /// This can be used to clear out a region so it can be used
    /// again.
    pub fn clear(&mut self) {
        unsafe { onig_region_clear(self.0) };
    }
}
