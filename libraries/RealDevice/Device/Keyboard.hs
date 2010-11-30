-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Authors: Adam Wick <awick@galois.com>
-- BANNEREND
module Device.Keyboard(
         initKeyboard
       )
 where

import Control.Concurrent.Chan
import Data.Bits
import Data.Char
import Data.Word
import Hypervisor.IOPorts
import Hypervisor.Port
import Hypervisor.Privileged

-- |Initialize the keyboard, returning a lazy infinite stream containing 
-- everything the user types. The up, down, left, right, page up, and page
-- down keys are translated into standard xterm escape codes. Some control
-- combinations (CTRL-C, -G, -H, -M, -E, -L, -J, -N, -I, -K, and -O) are
-- turned into their corresponding ASCII equivalents (BRK, BEL, BS, CR, ENQ,
-- FF, LF, SO, SP, TAB, VTAB, and SI, respectively). 
--
-- Control characters for letters are translated into ASCII values 1 - 26,
-- where 1 is control-a, 2 is control-b, etc.. Shifts have no effect on this
-- computation.
--
-- Control combined with any other character returns the standard value for
-- that character; the control is ignored.
--
initKeyboard :: IO [Char]
initKeyboard = do
  chan <- newChan
  kbp <- bindPhysicalIRQ 1 False
  setPortHandler kbp $ keyboard_interrupt chan
  sanitize_keys 0 0 `fmap` getChanContents chan

keyboard_interrupt :: Chan Word8 -> IO ()
keyboard_interrupt chan = do
  val <- in8 0x60
  sendEOI 1
  writeChan chan val

data KeyCode = KCChar Char
             | KCControl | KCShift 
             | KCUp | KCDown | KCLeft | KCRight | KCPgUp | KCPgDown
             | KCF1 | KCF2 | KCF3 | KCF4 | KCF5 | KCF6 | KCF7 | KCF8
             | KCF9 | KCF10 | KCF11 | KCF12 | KCF13 | KCF14 | KCF15 
             | KCF16 | KCF17
             | KCIgnore
  deriving (Show, Eq)

sanitize_keys :: Int -> Int -> [Word8] -> [Char]
sanitize_keys _ _ [] = error "End of keycode stream?!"
sanitize_keys scnt ccnt (b:rest)
  | kc == KCIgnore    = sanitize_keys scnt ccnt rest
  | upkey && is_shift = sanitize_keys (scnt - 1) ccnt rest
  |          is_shift = sanitize_keys (scnt + 1) ccnt rest
  | upkey && is_cntrl = sanitize_keys scnt (ccnt - 1) rest
  |          is_cntrl = sanitize_keys scnt (ccnt + 1) rest
  | upkey             = sanitize_keys scnt ccnt rest
  | ccnt > 0          = handle_control kc ++ sanitize_keys scnt ccnt rest
  | scnt > 0          = unwrap skc ++ sanitize_keys scnt ccnt rest
  | otherwise         = unwrap kc ++ sanitize_keys scnt ccnt rest
 where
  upkey = testBit b 7
  b' = fromIntegral $ clearBit b 7
  (kc, skc) = codes !! b'
  is_shift = kc == KCShift
  is_cntrl = kc == KCControl

unwrap :: KeyCode -> String
unwrap (KCChar x) = [x]
unwrap KCUp       = "\ESC[A"
unwrap KCDown     = "\ESC[B"
unwrap KCLeft     = "\ESC[D"
unwrap KCRight    = "\ESC[C"
unwrap KCPgUp     = "\ESC[5~"
unwrap KCPgDown   = "\ESC[6~"
unwrap KCF1       = "\ESC[[A"
unwrap KCF2       = "\ESC[[B"
unwrap KCF3       = "\ESC[[C"
unwrap KCF4       = "\ESC[[D"
unwrap KCF5       = "\ESC[[E"
unwrap KCF6       = "\ESC[17~"
unwrap KCF7       = "\ESC[18~"
unwrap KCF8       = "\ESC[19~"
unwrap KCF9       = "\ESC[20~"
unwrap KCF10      = "\ESC[21~"
unwrap KCF11      = "\ESC[23~"
unwrap KCF12      = "\ESC[24~"
unwrap KCF13      = "\ESC[25~"
unwrap KCF14      = "\ESC[26~"
unwrap KCF15      = "\ESC[28~"
unwrap KCF16      = "\ESC[29~"
unwrap KCF17      = "\ESC[31~"
unwrap x          = error $ "Bad value reached unwrap: " ++ show x

handle_control :: KeyCode -> String
handle_control c = 
 case unwrap c of
   "c" -> [chr 24]
   "h" -> [chr 7]
   "m" -> "\r"
   "e" -> [chr 5]
   "l" -> [chr 12]
   "j" -> [chr 10]
   "n" -> [chr 14]
   "i" -> "\t"
   "k" -> "\n"
   "o" -> [chr 15]
   (l:[]) | l `elem` ['a'..'z'] -> [chr $ 1 + (ord l - ord 'a')]
   res -> res

codes :: [(KeyCode, KeyCode)]
codes = [
        --  Normal              Shifted           
 {- 000 -} (KCIgnore,           KCIgnore),
 {- 001 -} (KCChar '\ESC',      KCChar '\ESC'),
 {- 002 -} (KCChar '1',         KCChar '!'),
 {- 003 -} (KCChar '2',         KCChar '@'),
 {- 004 -} (KCChar '3',         KCChar '#'),
 {- 005 -} (KCChar '4',         KCChar '$'),
 {- 006 -} (KCChar '5',         KCChar '%'),
 {- 007 -} (KCChar '6',         KCChar '^'),
 {- 008 -} (KCChar '7',         KCChar '&'),
 {- 009 -} (KCChar '8',         KCChar '*'),
 {- 010 -} (KCChar '9',         KCChar '('),
 {- 011 -} (KCChar '0',         KCChar ')'),
 {- 012 -} (KCChar '-',         KCChar '_'),
 {- 013 -} (KCChar '=',         KCChar '+'),
 {- 014 -} (KCChar '\b',        KCChar '\b'),
 {- 015 -} (KCChar '\t',        KCChar '\t'),
 {- 016 -} (KCChar 'q',         KCChar 'Q'),
 {- 017 -} (KCChar 'w',         KCChar 'W'),
 {- 018 -} (KCChar 'e',         KCChar 'E'),
 {- 019 -} (KCChar 'r',         KCChar 'R'),
 {- 020 -} (KCChar 't',         KCChar 'T'),
 {- 021 -} (KCChar 'y',         KCChar 'Y'),
 {- 022 -} (KCChar 'u',         KCChar 'U'),
 {- 023 -} (KCChar 'i',         KCChar 'I'),
 {- 024 -} (KCChar 'o',         KCChar 'O'),
 {- 025 -} (KCChar 'p',         KCChar 'P'),
 {- 026 -} (KCChar '[',         KCChar '{'),
 {- 027 -} (KCChar ']',         KCChar '}'),
 {- 028 -} (KCChar '\n',        KCChar '\n'),
 {- 029 -} (KCControl,          KCControl),
 {- 030 -} (KCChar 'a',         KCChar 'A'),
 {- 031 -} (KCChar 's',         KCChar 'S'),
 {- 032 -} (KCChar 'd',         KCChar 'D'),
 {- 033 -} (KCChar 'f',         KCChar 'F'),
 {- 034 -} (KCChar 'g',         KCChar 'G'),
 {- 035 -} (KCChar 'h',         KCChar 'H'),
 {- 036 -} (KCChar 'j',         KCChar 'J'),
 {- 037 -} (KCChar 'k',         KCChar 'K'),
 {- 038 -} (KCChar 'l',         KCChar 'L'),
 {- 039 -} (KCChar ';',         KCChar ':'),
 {- 040 -} (KCChar '\'',        KCChar '"'),
 {- 041 -} (KCChar '`',         KCChar '~'),
 {- 042 -} (KCShift,            KCShift),
 {- 043 -} (KCChar '\\',        KCChar '|'),
 {- 044 -} (KCChar 'z',         KCChar 'Z'),
 {- 045 -} (KCChar 'x',         KCChar 'X'),
 {- 046 -} (KCChar 'c',         KCChar 'C'),
 {- 047 -} (KCChar 'v',         KCChar 'V'),
 {- 048 -} (KCChar 'b',         KCChar 'B'),
 {- 049 -} (KCChar 'n',         KCChar 'N'),
 {- 050 -} (KCChar 'm',         KCChar 'M'),
 {- 051 -} (KCChar ',',         KCChar '<'),
 {- 052 -} (KCChar '.',         KCChar '>'),
 {- 053 -} (KCChar '/',         KCChar '?'),
 {- 054 -} (KCShift,            KCShift),
 {- 055 -} (KCChar '*',         KCChar '*'),
 {- 056 -} (KCIgnore,           KCIgnore),
 {- 057 -} (KCChar ' ',         KCChar ' '),
 {- 058 -} (KCIgnore,           KCIgnore), -- CAPS LOCK (FIXME)
 {- 059 -} (KCF1,               KCF1),
 {- 060 -} (KCF2,               KCF2),
 {- 061 -} (KCF3,               KCF3),
 {- 062 -} (KCF4,               KCF5),
 {- 063 -} (KCF5,               KCF5),
 {- 064 -} (KCF6,               KCF6),
 {- 065 -} (KCF7,               KCF7),
 {- 066 -} (KCF8,               KCF8),
 {- 067 -} (KCF9,               KCF9),
 {- 068 -} (KCF10,              KCF10),
 {- 069 -} (KCIgnore,           KCIgnore),
 {- 070 -} (KCIgnore,           KCIgnore),
 {- 071 -} (KCChar '7',         KCChar '7'),
 {- 072 -} (KCChar '8',         KCChar '8'),
 {- 073 -} (KCChar '9',         KCChar '9'),
 {- 074 -} (KCChar '-',         KCChar '-'),
 {- 075 -} (KCChar '4',         KCChar '4'),
 {- 076 -} (KCChar '5',         KCChar '5'),
 {- 077 -} (KCChar '6',         KCChar '6'),
 {- 078 -} (KCChar '+',         KCChar '+'),
 {- 079 -} (KCChar '1',         KCChar '1'),
 {- 080 -} (KCChar '2',         KCChar '2'),
 {- 081 -} (KCChar '3',         KCChar '3'),
 {- 082 -} (KCChar '0',         KCChar '0'),
 {- 083 -} (KCChar '.',         KCChar '.'),
 {- 084 -} (KCIgnore,           KCIgnore),
 {- 085 -} (KCIgnore,           KCIgnore),
 {- 086 -} (KCChar '<',         KCChar '>'),
 {- 087 -} (KCF11,              KCF11),
 {- 088 -} (KCF12,              KCF12),
 {- 089 -} (KCIgnore,           KCIgnore),
 {- 090 -} (KCIgnore,           KCIgnore),
 {- 091 -} (KCIgnore,           KCIgnore),
 {- 092 -} (KCIgnore,           KCIgnore),
 {- 093 -} (KCIgnore,           KCIgnore),
 {- 094 -} (KCIgnore,           KCIgnore),
 {- 095 -} (KCIgnore,           KCIgnore),
 {- 096 -} (KCControl,          KCControl), -- Probably not right
 {- 097 -} (KCControl,          KCControl),
 {- 098 -} (KCChar '/',         KCChar '/'),
 {- 099 -} (KCIgnore,           KCIgnore),
 {- 100 -} (KCIgnore,           KCIgnore),
 {- 101 -} (KCIgnore,           KCIgnore), -- BREAK
 {- 102 -} (KCIgnore,           KCIgnore),
 {- 103 -} (KCUp,               KCUp), 
 {- 104 -} (KCPgUp,             KCPgUp),
 {- 105 -} (KCLeft,             KCLeft),
 {- 106 -} (KCRight,            KCRight),
 {- 107 -} (KCIgnore,           KCIgnore),
 {- 108 -} (KCDown,             KCDown),
 {- 109 -} (KCPgDown,           KCPgDown),
 {- 110 -} (KCIgnore,           KCIgnore),
 {- 111 -} (KCIgnore,           KCIgnore),
 {- 112 -} (KCIgnore,           KCIgnore),
 {- 113 -} (KCF13,              KCF13),
 {- 114 -} (KCF14,              KCF14),
 {- 115 -} (KCF15,              KCF15),
 {- 116 -} (KCF16,              KCF16),
 {- 117 -} (KCF17,              KCF17),
 {- 118 -} (KCIgnore,           KCIgnore),
 {- 119 -} (KCIgnore,           KCIgnore),
 {- 120 -} (KCIgnore,           KCIgnore),
 {- 121 -} (KCIgnore,           KCIgnore),
 {- 122 -} (KCIgnore,           KCIgnore),
 {- 123 -} (KCIgnore,           KCIgnore),
 {- 124 -} (KCIgnore,           KCIgnore),
 {- 125 -} (KCIgnore,           KCIgnore),
 {- 126 -} (KCIgnore,           KCIgnore),
 {- 127 -} (KCIgnore,           KCIgnore)
 ]
