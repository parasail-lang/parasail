-- $Source: /home/projects/ada/cvs-repository/rts/portable/ada.strings.maps.spc,v $
-- $Revision: 245876 $ $Date: 2009-07-23 15:34:40 -0400 (Thu, 23 Jul 2009) $ $Author: stt $
--        Copyright (C) 1995 by Intermetrics, Inc.

with Ada.Characters.Latin_1;
with Standard.*;

package Ada.Strings.Maps is

   pragma Suppress( Elaboration_Check );
   pragma Preelaborate (Maps);

   -- Representation for a set of character values:
   type Character_Set is private;

   Null_Set : constant Character_Set;

   type Character_Range is
     record
      Low  : Character;
      High : Character;
   end record;
   -- Represents Character range Low .. High

   type Character_Ranges is array (Positive range <>) of Character_Range;

   function To_Set    (Ranges : in Character_Ranges) return Character_Set;

   function To_Set    (Span   : in Character_Range)  return Character_Set;

   function To_Ranges (Set    : in Character_Set)    return Character_Ranges;

   function "="   (Left, Right : in Character_Set) return Boolean;

   function "not" (Right       : in Character_Set) return Character_Set;
   function "and" (Left, Right : in Character_Set) return Character_Set;
   function "or"  (Left, Right : in Character_Set) return Character_Set;
   function "xor" (Left, Right : in Character_Set) return Character_Set;
   function "-"   (Left, Right : in Character_Set) return Character_Set;

   function Is_In (Element : in Character;
                   Set     : in Character_Set)
      return Boolean;

   function Is_Subset (Elements : in Character_Set;
                       Set      : in Character_Set)
      return Boolean;

   function "<=" (Left  : in Character_Set;
                  Right : in Character_Set)
      return Boolean renames Is_Subset;

   -- Alternative representation for a set of character values:
   subtype Character_Sequence is String;

   function To_Set (Sequence  : in Character_Sequence) return Character_Set;

   function To_Set (Singleton : in Character)          return Character_Set;

   function To_Sequence (Set : in Character_Set) return Character_Sequence;

   -- Representation for a character to character mapping:
   type Character_Mapping is private;

   function Value (Map     : in Character_Mapping;
                   Element : in Character)
      return Character;

   Identity : constant Character_Mapping;

   function To_Mapping (From, To : in Character_Sequence)
      return Character_Mapping;

   function To_Domain (Map  : in Character_Mapping)
      return Character_Sequence;

   function To_Range (Map  : in Character_Mapping)
      return Character_Sequence;

   type Character_Mapping_Function is
      access function (From : in Character) return Character;

--  private

   use Ada.Characters.Latin_1;

   type Character_Set is array (Character) of Boolean;
   pragma pack (Character_Set);

   Null_Set : constant Character_Set := (others => False);

   type Character_Mapping is array (Character) of Character;

   Identity : constant Character_Mapping :=
     (
      -- Control characters:

      NUL                         ,  --             0
      SOH                         ,  --             1
      STX                         ,  --             2
      ETX                         ,  --             3
      EOT                         ,  --             4
      ENQ                         ,  --             5
      ACK                         ,  --             6
      BEL                         ,  --             7
      BS                          ,  --             8
      HT                          ,  --             9
      LF                          ,  --            10
      VT                          ,  --            11
      FF                          ,  --            12
      CR                          ,  --            13
      SO                          ,  --            14
      SI                          ,  --            15

      DLE                         ,  --            16
      DC1                         ,  --            17
      DC2                         ,  --            18
      DC3                         ,  --            19
      DC4                         ,  --            20
      NAK                         ,  --            21
      SYN                         ,  --            22
      ETB                         ,  --            23
      CAN                         ,  --            24
      EM                          ,  --            25
      SUB                         ,  --            26
      ESC                         ,  --            27
      FS                          ,  --            28
      GS                          ,  --            29
      RS                          ,  --            30
      US                          ,  --            31

      -- ISO 646 graphic characters:

      Space                       ,  -- ' '        32
      Exclamation                 ,  -- '!'        33
      Quotation                   ,  -- '"'        34
      Number_Sign                 ,  -- '#'        35
      Dollar_Sign                 ,  -- '$'        36
      Percent_Sign                ,  -- '%'        37
      Ampersand                   ,  -- '&'        38
      Apostrophe                  ,  -- '''        39
      Left_Parenthesis            ,  -- '('        40
      Right_Parenthesis           ,  -- ')'        41
      Asterisk                    ,  -- '*'        42
      Plus_Sign                   ,  -- '+'        43
      Comma                       ,  -- ','        44
      Hyphen                      ,  -- '-'        45
      Full_Stop                   ,  -- '.'        46
      Solidus                     ,  -- '/'        47

      '0'                         ,  --            48
      '1'                         ,  --            49
      '2'                         ,  --            50
      '3'                         ,  --            51
      '4'                         ,  --            52
      '5'                         ,  --            53
      '6'                         ,  --            54
      '7'                         ,  --            55
      '8'                         ,  --            56
      '9'                         ,  --            57
      Colon                       ,  -- ':'        58
      Semicolon                   ,  -- ';'        59
      Less_Than_Sign              ,  -- '<'        60
      Equals_Sign                 ,  -- '='        61
      Greater_Than_Sign           ,  -- '>'        62
      Question                    ,  -- '?'        63

      Commercial_At               ,  -- '@'        64
      'A'                         ,  --            65
      'B'                         ,  --            66
      'C'                         ,  --            67
      'D'                         ,  --            68
      'E'                         ,  --            69
      'F'                         ,  --            70
      'G'                         ,  --            71
      'H'                         ,  --            72
      'I'                         ,  --            73
      'J'                         ,  --            74
      'K'                         ,  --            75
      'L'                         ,  --            76
      'M'                         ,  --            77
      'N'                         ,  --            78
      'O'                         ,  --            79

      'P'                         ,  --            80
      'Q'                         ,  --            81
      'R'                         ,  --            82
      'S'                         ,  --            83
      'T'                         ,  --            84
      'U'                         ,  --            85
      'V'                         ,  --            86
      'W'                         ,  --            87
      'X'                         ,  --            88
      'Y'                         ,  --            89
      'Z'                         ,  --            90
      Left_Square_Bracket         ,  -- '['        91
      Reverse_Solidus             ,  -- '\'        92
      Right_Square_Bracket        ,  -- ']'        93
      Circumflex                  ,  -- '^'        94
      Low_Line                    ,  -- '_'        95

      Grave                       ,  -- '`'        96
      LC_A                        ,  -- 'a'        97
      LC_B                        ,  -- 'b'        98
      LC_C                        ,  -- 'c'        99
      LC_D                        ,  -- 'd'       100
      LC_E                        ,  -- 'e'       101
      LC_F                        ,  -- 'f'       102
      LC_G                        ,  -- 'g'       103
      LC_H                        ,  -- 'h'       104
      LC_I                        ,  -- 'i'       105
      LC_J                        ,  -- 'j'       106
      LC_K                        ,  -- 'k'       107
      LC_L                        ,  -- 'l'       108
      LC_M                        ,  -- 'm'       109
      LC_N                        ,  -- 'n'       110
      LC_O                        ,  -- 'o'       111

      LC_P                        ,  -- 'p'       112
      LC_Q                        ,  -- 'q'       113
      LC_R                        ,  -- 'r'       114
      LC_S                        ,  -- 's'       115
      LC_T                        ,  -- 't'       116
      LC_U                        ,  -- 'u'       117
      LC_V                        ,  -- 'v'       118
      LC_W                        ,  -- 'w'       119
      LC_X                        ,  -- 'x'       120
      LC_Y                        ,  -- 'y'       121
      LC_Z                        ,  -- 'z'       122
      Left_Curly_Bracket          ,  -- '{'       123
      Vertical_Line               ,  -- '|'       124
      Right_Curly_Bracket         ,  -- '}'       125
      Tilde                       ,  -- '~'       126
      DEL                         ,  --           127

      -- ISO 6429 control characters:

      Reserved_128                ,  --           128
      Reserved_129                ,  --           129
      BPH                         ,  --           130
      NBH                         ,  --           131
      Reserved_132                ,  --           132
      NEL                         ,  --           133
      SSA                         ,  --           134
      ESA                         ,  --           135
      HTS                         ,  --           136
      HTJ                         ,  --           137
      VTS                         ,  --           138
      PLD                         ,  --           139
      PLU                         ,  --           140
      RI                          ,  --           141
      SS2                         ,  --           142
      SS3                         ,  --           143

      DCS                         ,  --           144
      PU1                         ,  --           145
      PU2                         ,  --           146
      STS                         ,  --           147
      CCH                         ,  --           148
      MW                          ,  --           149
      SPA                         ,  --           150
      EPA                         ,  --           151
      SOS                         ,  --           152
      Reserved_153                ,  --           153
      SCI                         ,  --           154
      CSI                         ,  --           155
      ST                          ,  --           156
      OSC                         ,  --           157
      PM                          ,  --           158
      APC                         ,  --           159

      -- Other graphic characters:

      No_Break_Space              ,  --           160
      Inverted_Exclamation        ,  --           161
      Cent_Sign                   ,  --           162
      Pound_Sign                  ,  --           163
      Currency_Sign               ,  --           164
      Yen_Sign                    ,  --           165
      Broken_Bar                  ,  --           166
      Section_Sign                ,  --           167
      Diaeresis                   ,  --           168
      Copyright_Sign              ,  --           169
      Feminine_Ordinal_Indicator  ,  --           170
      Left_Angle_Quotation        ,  --           171
      Not_Sign                    ,  --           172
      Soft_Hyphen                 ,  --           173
      Registered_Trade_Mark_Sign  ,  --           174
      Macron                      ,  --           175

      Degree_Sign                 ,  --           176
      Plus_Minus_Sign             ,  --           177
      Superscript_Two             ,  --           178
      Superscript_Three           ,  --           179
      Acute                       ,  --           180
      Micro_Sign                  ,  --           181
      Pilcrow_Sign                ,  --           182
      Middle_Dot                  ,  --           183
      Cedilla                     ,  --           184
      Superscript_One             ,  --           185
      Masculine_Ordinal_Indicator ,  --           186
      Right_Angle_Quotation       ,  --           187
      Fraction_One_Quarter        ,  --           188
      Fraction_One_Half           ,  --           189
      Fraction_Three_Quarters     ,  --           190
      Inverted_Question           ,  --           191

      UC_A_Grave                  ,  --           192
      UC_A_Acute                  ,  --           193
      UC_A_Circumflex             ,  --           194
      UC_A_Tilde                  ,  --           195
      UC_A_Diaeresis              ,  --           196
      UC_A_Ring                   ,  --           197
      UC_AE_Diphthong             ,  --           198
      UC_C_Cedilla                ,  --           199
      UC_E_Grave                  ,  --           200
      UC_E_Acute                  ,  --           201
      UC_E_Circumflex             ,  --           202
      UC_E_Diaeresis              ,  --           203
      UC_I_Grave                  ,  --           204
      UC_I_Acute                  ,  --           205
      UC_I_Circumflex             ,  --           206
      UC_I_Diaeresis              ,  --           207

      UC_Icelandic_Eth            ,  --           208
      UC_N_Tilde                  ,  --           209
      UC_O_Grave                  ,  --           210
      UC_O_Acute                  ,  --           211
      UC_O_Circumflex             ,  --           212
      UC_O_Tilde                  ,  --           213
      UC_O_Diaeresis              ,  --           214
      Multiplication_Sign         ,  --           215
      UC_O_Oblique_Stroke         ,  --           216
      UC_U_Grave                  ,  --           217
      UC_U_Acute                  ,  --           218
      UC_U_Circumflex             ,  --           219
      UC_U_Diaeresis              ,  --           220
      UC_Y_Acute                  ,  --           221
      UC_Icelandic_Thorn          ,  --           222
      LC_German_Sharp_S           ,  --           223

      LC_A_Grave                  ,  --           224
      LC_A_Acute                  ,  --           225
      LC_A_Circumflex             ,  --           226
      LC_A_Tilde                  ,  --           227
      LC_A_Diaeresis              ,  --           228
      LC_A_Ring                   ,  --           229
      LC_AE_Diphthong             ,  --           230
      LC_C_Cedilla                ,  --           231
      LC_E_Grave                  ,  --           232
      LC_E_Acute                  ,  --           233
      LC_E_Circumflex             ,  --           234
      LC_E_Diaeresis              ,  --           235
      LC_I_Grave                  ,  --           236
      LC_I_Acute                  ,  --           237
      LC_I_Circumflex             ,  --           238
      LC_I_Diaeresis              ,  --           239

      LC_Icelandic_Eth            ,  --           240
      LC_N_Tilde                  ,  --           241
      LC_O_Grave                  ,  --           242
      LC_O_Acute                  ,  --           243
      LC_O_Circumflex             ,  --           244
      LC_O_Tilde                  ,  --           245
      LC_O_Diaeresis              ,  --           246
      Division_Sign               ,  --           247
      LC_O_Oblique_Stroke         ,  --           248
      LC_U_Grave                  ,  --           249
      LC_U_Acute                  ,  --           250
      LC_U_Circumflex             ,  --           251
      LC_U_Diaeresis              ,  --           252
      LC_Y_Acute                  ,  --           253
      LC_Icelandic_Thorn          ,  --           254
      LC_Y_Diaeresis              ); --           255

end Ada.Strings.Maps;

-- From MonoCM file ada.strings.maps.spc,v.
