#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[test]
fn test() {
    let _ = unsafe { onig_init() };
    let mut buffer = std::mem::MaybeUninit::uninit();
    let mut einfo = std::mem::MaybeUninit::uninit();
    let s = "ab|c";
    let pattern: *const u8 = s.as_ptr() as _;
    let pattern_end = unsafe { pattern.add(s.len()) };
    let r = unsafe {
        onig_new(
            &mut buffer.as_mut_ptr(),
            pattern,
            pattern_end,
            ONIG_OPTION_NONE,
            &OnigEncodingUTF_8 as _,
            &OnigSyntaxRuby as _,
            einfo.as_mut_ptr(),
        )
    };
    if r != ONIG_NORMAL as _ {
        let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
        unsafe {
            onig_error_code_to_str(s.as_mut_ptr(), r as _, &einfo);
        }
        let err = str::from_utf8(&s).unwrap();
        eprintln!("ERROR: {err}");
        std::process::exit(-1);
    }

    let s = "sub account";
    let str_start = s.as_ptr();
    let str_end = unsafe { str_start.add(s.len()) };
    let range_start = str_start;
    let range_end = str_end;
    let region = unsafe { onig_region_new() };

    let r = unsafe {
        onig_search(
            buffer.as_mut_ptr(),
            str_start,
            str_end,
            range_start,
            range_end,
            region,
            ONIG_OPTION_NONE,
        )
    };

    let region = unsafe { &*region };
    if r >= 0 {
        eprintln!(
            "Match at:  - ", /*unsafe { *region.beg }, unsafe {
                                 *region.end
                             }*/
        );
    } else if r == ONIG_MISMATCH as _ {
        eprintln!("No match.");
    } else {
        let mut s = [0; ONIG_MAX_ERROR_MESSAGE_LEN as usize];
        unsafe {
            onig_error_code_to_str(s.as_mut_ptr(), r as _, &einfo);
        }
        let err = str::from_utf8(&s).unwrap();
        eprintln!("ERROR: {err}");
        std::process::exit(-1);
    }

    let buffer = unsafe { buffer.assume_init() };
    eprintln!("buffer: {buffer:?}");
}
/*static int
x0(int no, char* pattern_arg, char* str_arg,
   int start_offset, int expected_from, int expected_to, int backward)
{
  region = onig_region_new();

  end   = str + strlen((char* )str);
  if (backward) {
    start = end + start_offset;
    range = str;
  }
  else {
    start = str + start_offset;
    range = end;
  }
  r = onig_search(reg, str, end, start, range, region, ONIG_OPTION_NONE);
  if (r >= 0) {
    result(no, region->beg[0], region->end[0], expected_from, expected_to);
  }
  else if (r == ONIG_MISMATCH) {
    result(no, r, -1, expected_from, expected_to);
  }
  else { /* error */
    OnigUChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
    onig_error_code_to_str(s, r);
    fprintf(stderr, "ERROR: %s\n", s);
    return -1;
  }

  onig_region_free(region, 1 /* 1:free self, 0:free contents only */);
  onig_free(reg);
  return 0;
} */
