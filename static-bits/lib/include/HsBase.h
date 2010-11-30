// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
HsBool stg_gtWord64 (HsWord64, HsWord64);
HsBool stg_geWord64 (HsWord64, HsWord64);
HsBool stg_eqWord64 (HsWord64, HsWord64);
HsBool stg_neWord64 (HsWord64, HsWord64);
HsBool stg_ltWord64 (HsWord64, HsWord64);
HsBool stg_leWord64 (HsWord64, HsWord64);

HsBool stg_gtInt64 (HsInt64, HsInt64);
HsBool stg_geInt64 (HsInt64, HsInt64);
HsBool stg_eqInt64 (HsInt64, HsInt64);
HsBool stg_neInt64 (HsInt64, HsInt64);
HsBool stg_ltInt64 (HsInt64, HsInt64);
HsBool stg_leInt64 (HsInt64, HsInt64);

HsWord64 stg_remWord64  (HsWord64, HsWord64);
HsWord64 stg_quotWord64 (HsWord64, HsWord64);

HsInt64 stg_remInt64    (HsInt64, HsInt64);
HsInt64 stg_quotInt64   (HsInt64, HsInt64);
HsInt64 stg_negateInt64 (HsInt64);
HsInt64 stg_plusInt64   (HsInt64, HsInt64);
HsInt64 stg_minusInt64  (HsInt64, HsInt64);
HsInt64 stg_timesInt64  (HsInt64, HsInt64);

HsWord64 stg_and64  (HsWord64, HsWord64);
HsWord64 stg_or64   (HsWord64, HsWord64);
HsWord64 stg_xor64  (HsWord64, HsWord64);
HsWord64 stg_not64  (HsWord64);

HsWord64 stg_uncheckedShiftL64   (HsWord64, HsInt);
HsWord64 stg_uncheckedShiftRL64  (HsWord64, HsInt);
HsInt64  stg_uncheckedIShiftL64  (HsInt64, HsInt);
HsInt64  stg_uncheckedIShiftRA64 (HsInt64, HsInt);
HsInt64  stg_uncheckedIShiftRL64 (HsInt64, HsInt);

HsInt64  stg_intToInt64    (HsInt);
HsInt    stg_int64ToInt    (HsInt64);
HsWord64 stg_int64ToWord64 (HsInt64);
HsWord64 stg_wordToWord64  (HsWord);
HsWord   stg_word64ToWord  (HsWord64);
HsInt64  stg_word64ToInt64 (HsWord64);

HsWord64 stg_integerToWord64 (HsInt sa, StgByteArray /* Really: mp_limb_t* */ da);
HsInt64  stg_integerToInt64 (HsInt sa, StgByteArray /* Really: mp_limb_t* */ da);
