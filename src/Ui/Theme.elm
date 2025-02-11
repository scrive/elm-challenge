module Ui.Theme exposing (..)

import Element as E


white = E.rgb255 255 255 255
black = E.rgb255 0 0 0


-- COLORS


gray25  = E.rgb255 252 252 253 -- #fcfcfd
gray50  = E.rgb255 249 250 251 -- #f9fafb
gray100 = E.rgb255 243 244 247 -- #f3f4f7
gray200 = E.rgb255 235 236 240 -- #ebecf0
gray300 = E.rgb255 208 213 221 -- #d0d5dd
gray400 = E.rgb255 152 162 180 -- #98a2b4
gray450 = E.rgb255 114 127 150 -- #727f96
gray500 = E.rgb255 102 112 133 -- #667085
gray600 = E.rgb255 71 84 104   -- #475468
gray700 = E.rgb255 52 64 85    -- #344055
gray800 = E.rgb255 29 41 57    -- #1d2939
gray900 = E.rgb255 16 24 40    -- #101828


blue25  = E.rgb255 245 250 255 -- #f5faff
blue50  = E.rgb255 239 248 255 -- #eff8ff
blue100 = E.rgb255 209 233 255 -- #d1e9ff
blue200 = E.rgb255 178 221 255 -- #b2ddff
blue300 = E.rgb255 132 202 255 -- #84caff
blue400 = E.rgb255 83 177 253  -- #53b1fd
blue500 = E.rgb255 46 144 250  -- #2e90fa
blue600 = E.rgb255 21 112 239  -- #1570ef
blue700 = E.rgb255 23 92 211   -- #175cd3
blue800 = E.rgb255 24 73 169   -- #1849a9
blue900 = E.rgb255 25 65 133   -- #194185


red25  = E.rgb255 255 251 250 -- #fffbfa
red50  = E.rgb255 254 243 242 -- #fef3f2
red100 = E.rgb255 254 228 226 -- #fee4e2
red200 = E.rgb255 254 205 202 -- #fecdca
red300 = E.rgb255 253 162 155 -- #fda29b
red400 = E.rgb255 249 112 102 -- #f97066
red500 = E.rgb255 240 79 56   -- #f04f38
red600 = E.rgb255 217 45 32   -- #d92d20
red700 = E.rgb255 180 35 24   -- #b42318
red800 = E.rgb255 145 32 24   -- #912018
red900 = E.rgb255 122 39 26   -- #7a271a



-- SPACE


space1 = 4     -- 16 * 0.25
space2 = 8     -- 16 * 0.5
space3 = 12    -- 16 * 0.75
space4 = 16    -- 16 * 1
space5 = 20    -- 16 * 1.25
space6 = 24    -- 16 * 1.5
space7 = 32    -- 16 * 2
space8 = 40    -- 16 * 2.5
space9 = 48    -- 16 * 3
space10 = 64   -- 16 * 4
space11 = 80   -- 16 * 5
space12 = 96   -- 16 * 6
space13 = 128  -- 16 * 8
space14 = 160  -- 16 * 10
space15 = 192  -- 16 * 12
space16 = 256  -- 16 * 16
space17 = 384  -- 16 * 24
space18 = 512  -- 16 * 32
space19 = 640  -- 16 * 40
space20 = 768  -- 16 * 48
space21 = 896  -- 16 * 56
space22 = 1024 -- 16 * 64
space23 = 1280 -- 16 * 80

desktopWidth = space19

fontSize0 = 10
fontSize1 = 12
fontSize2 = 14
fontSize3 = 16
fontSize4 = 18
fontSize5 = 20
fontSize6 = 24
fontSize7 = 30
fontSize8 = 36
fontSize9 = 48
fontSize10 = 60
fontSize11 = 72