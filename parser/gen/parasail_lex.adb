
pragma Style_Checks (Off);
package body parasail_lex is

  --  Indicates whether outdenting should expect a perfect col-count match
  Col_Match_Expected : Boolean := True;

  Max_Depth : constant := 100;
  Indent_Stack : array(Positive range 1 .. Max_Depth) of Natural :=
    (others => 0);
  Bracketing_Token_Stack : array(Positive range 1 .. Max_Depth) of Token :=
    (others => Error);

  Max_Unquote_Depth : constant := 100;
  Unquote_Stack : array (Positive range 1 .. Max_Unquote_Depth) of Natural :=
    (others => 0);
  --  Stack of Paren counts at point when an unquote is started.

  procedure ECHO_L(YYT : String := yytext) is
  --
  -- Local version of the  define string.
  -- 
  begin
     Echo_Token (YYT);
     --  We normally expect an exact match
     Col_Match_Expected := True;
  end ECHO_L;

  procedure Init is
  begin
     --  Reset indent level stack.
     Top := 0;
  end Init;

  function Create_Token(Text : String := yytext) return YYSType is
     Src_Pos : Source_Position := Cur_Source_Pos;
  begin
     --  Point to beginning of token
     Src_Pos.Col := Column_Number'Max (1, Src_Pos.Col - Text'Length + 1);
     --  TBD: "gcc" prefers column count rather than character count

     if Debug_Indent and then Expecting_Indent then
        Text_IO.Put (" [CT: indent off] ");
     end if;
     Expecting_Indent := False;

     --  Create a token for the parser.
     return (One_Token, Src_Pos, String_Lookup(Text));
  end Create_Token;
     
function YYLex return Token is
subtype short is integer range -32768..32767;
    yy_act : integer;
    yy_c : short;

-- returned upon end-of-file
YY_END_TOK : constant integer := 0;
YY_END_OF_BUFFER : constant := 161;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
TICK : constant := 1;
CHARLIT : constant := 2;
AFTER_IMPLEMENTS : constant := 3;
AFTER_UNQUOTE : constant := 4;
OUTDENTING : constant := 5;
RESCANNING : constant := 6;
yy_accept : constant array(0..554) of short :=
    (   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  161,  159,  149,  158,  159,  159,
      159,  114,  115,  124,  125,  126,  127,  128,  129,  139,
      139,  131,  132,  118,  136,  119,  135,  137,  116,  117,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  120,  134,  122,    6,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  134,  112,  159,    6,  160,
      113,  133,  155,  155,  155,  155,  155,  155,  160,  160,
      160,  160,  160,  160,  160,  160,  160,  160,  160,  149,

      158,    0,    0,    0,   81,    0,  146,    0,  147,  138,
       85,  101,  111,   99,  148,  100,   95,  111,   89,  102,
        0,    0,  139,    0,    0,    0,    0,    0,  130,    0,
      111,    0,   93,   86,    0,   84,   83,    0,  123,   80,
       94,   79,   82,    0,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,   45,  137,   48,   50,  137,  137,  137,  137,  137,
      137,   59,   60,   63,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  121,  104,   98,    6,    0,
        0,    0,    0,    0,    0,  137,  137,  137,  137,  137,

      137,  137,  137,   48,  137,    6,  137,  137,   98,    0,
        0,    6,    0,  154,    0,    0,    0,  153,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,   15,    0,
        0,   97,  147,  138,  103,  148,   92,    0,  140,  144,
        0,  139,  143,    0,   91,  109,   87,   88,  105,   96,
      110,   17,   19,   20,  137,  137,  137,  137,  137,   43,
      137,  137,  137,   35,  137,  137,  137,   40,  137,  137,
      137,  137,  137,  137,  137,   54,   56,   57,  137,   61,
      107,  137,  137,   66,   67,  137,  137,  137,  137,  137,
      137,   75,  137,  137,   78,    4,    0,    0,    0,    0,

        0,   17,  137,  137,    6,    6,  137,  137,  137,  137,
       56,    6,  137,    3,  145,    0,    0,    0,    0,    0,
        0,    0,    0,   12,   10,    0,    0,    0,    0,   55,
       16,    0,  141,    0,    0,    0,  140,    0,    0,  144,
        0,  139,    0,  143,    0,    0,   90,  137,  106,  137,
      137,   23,  137,  137,  137,  137,   28,   31,   29,  137,
      137,   36,  137,  137,  137,   42,  137,  137,  137,  137,
      137,  137,   53,   58,  137,  137,  137,  137,  137,   70,
       72,   73,  137,  137,   77,  108,    0,    1,    0,  137,
      137,  137,  137,    6,  137,  137,   72,    0,    0,    0,

      152,    0,    0,    0,    0,    0,   11,    0,    0,   71,
        0,    0,    0,  140,  137,   21,   22,   24,  137,   26,
      137,    0,  137,   33,   34,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,   74,
       76,    0,  137,    6,  137,  137,  137,  137,    5,    0,
      150,    0,    8,    0,    0,    0,    0,    0,  141,  142,
        0,    0,  140,    0,  137,  137,  137,    0,   30,  137,
      137,  137,   44,  137,   47,  137,   51,   52,  137,  137,
       65,   68,  137,    0,  137,  137,  137,    6,  137,    0,
        0,    0,    0,   13,    0,  141,    0,    0,  137,  137,

      137,   32,   38,   39,   41,  137,  137,  137,   64,   69,
        0,  137,  137,   38,  137,    0,    0,    0,   37,    0,
        0,  142,   18,  137,   27,  137,  137,   62,    0,    6,
      137,    2,  137,    0,    7,    0,    0,  142,    0,  137,
      137,   49,  137,    6,    0,    0,   14,   25,   46,    6,
        0,    9,  151,    0
    ) ;

yy_ec : constant array(ASCII.NUL..Character'Last) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    2,    2,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    1,    1,    1,    8,    9,
       10,   11,   12,   13,   14,   15,   16,   17,   18,   18,
       18,   18,   18,   18,   18,   18,   18,   19,   20,   21,
       22,   23,   24,    1,   25,   26,   25,   25,   27,   25,
       28,   28,   28,   28,   28,   28,   28,   28,   28,   28,
       28,   28,   28,   28,   28,   28,   28,   29,   28,   28,
       30,   31,   32,    1,   33,   34,   35,   36,   37,   38,

       39,   40,   41,   42,   43,   28,   44,   45,   46,   47,
       48,   49,   50,   51,   52,   53,   54,   55,   56,   57,
       58,   28,   59,   60,   61,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    ) ;

yy_meta : constant array(0..61) of short :=
    (   0,
        1,    2,    3,    2,    1,    1,    4,    5,    1,    1,
        6,    1,    1,    1,    7,    1,    8,    8,    1,    1,
        1,    6,    1,    1,    9,    9,    9,   10,   10,    1,
        1,    1,   11,    1,    9,    9,    9,    9,    9,    9,
       10,   10,   10,   10,   10,   10,   10,   10,   10,   10,
       10,   10,   10,   10,   10,   10,   10,   10,    1,    1,
        1
    ) ;

yy_base : constant array(0..573) of short :=
    (   0,
        0,   60,  855,  854,  853,  852,  851,   61,   57,   58,
       69,   72,    0,   98,  858,  864,   74,  864,  835,   90,
       66,  864,  864,   60,   70,  864,   87,   72,  112,  152,
      135,  180,  864,  133,   82,  104,  864,    0,  864,  864,
      128,  118,   95,  817,  160,  112,  810,  125,   96,  806,
      150,  157,  802,  798,  812,  802,   44,  802,  813,  158,
      799,  823,  101,  864,  216,  167,  176,  806,  181,  132,
      189,  183,  193,  805,  192,  188,  864,  812,  211,  864,
      864,  864,  864,    0,  796,  802,  798,  779,  802,   46,
      798,  192,  782,  199,  796,  785,  794,  790,  771,  250,

      864,  254,  816,  813,  864,  237,  864,  825,    0,    0,
      805,  864,  864,  864,    0,  864,  864,  811,  804,  864,
        0,  255,  260,  262,  271,    0,  264,  288,  864,  258,
      805,  807,  864,  864,  807,  799,  274,  798,  864,  796,
      864,  864,  864,  796,    0,  765,  771,  777,  773,  765,
      760,  776,  763,  769,  771,  104,  769,  257,  755,  758,
      756,    0,  754,  749,    0,  755,  257,  762,  743,  745,
      752,    0,  743,  773,  751,  754,  261,  746,  752,  741,
      736,  737,  744,  733,  734,  864,  864,  864,    0,    0,
      727,  731,  743,  739,  720,  727,  743,  730,  736,  737,

      259,  727,  724,  719,  715,  717,  269,  730,  315,  760,
      313,    0,  719,  864,  717,  709,  725,  864,  711,  727,
      714,  720,  721,  709,  710,  707,  702,  698,  864,  713,
      713,  864,    0,    0,  864,    0,  864,  296,  308,  310,
      313,  315,  718,  747,  728,  864,  864,  864,  864,  864,
      864,  695,    0,  725,  703,  708,  705,  691,  286,    0,
      700,  701,  231,  697,  686,  690,  698,  680,  698,  698,
      289,  694,  696,  687,  681,    0,    0,    0,  684,  685,
      864,  672,  672,    0,    0,  671,  685,  684,  675,  682,
      677,    0,  674,  676,  695,  864,  667,  342,  659,  675,

      347,  660,  660,  303,    0,  668,  662,  672,  309,  669,
      356,    0,  660,  864,  864,  699,  654,  659,  359,  656,
      649,  649,  663,  864,  864,  651,  661,  649,  657,  864,
      864,  648,  259,    0,  357,  358,  349,  357,  335,  360,
      362,  366,  368,  661,  660,  689,  864,  640,  864,  643,
      645,    0,  636,  633,  633,  642,    0,    0,  338,  644,
      643,    0,  631,  634,  645,    0,  644,  639,  626,  625,
      637,  635,    0,    0,  625,  637,  632,  619,  618,    0,
        0,    0,  623,  628,    0,  864,  618,  864,  618,  613,
      611,  608,  610,    0,  609,  608,  392,  651,  398,  618,

      864,  401,  605,  603,  600,  602,  864,  601,  600,  864,
      394,  380,  380,  392,  615,    0,    0,    0,  598,    0,
      601,  604,  606,    0,    0,  592,  606,  592,  597,  595,
      587,  599,  603,  599,  589,  582,  596,  586,  580,    0,
        0,  580,  595,    0,  578,  575,  574,  586,  864,  579,
      864,  589,  864,  572,  569,  568,  580,  397,  399,  380,
      385,  413,  404,  406,  582,  567,  563,  575,    0,  560,
      559,  572,    0,  566,    0,  567,    0,    0,  566,  561,
        0,    0,  547,  529,  541,  526,  518,    0,  517,  498,
      492,  466,  454,  864,  470,  409,  411,  421,  451,  464,

      462,  864,    0,    0,    0,  418,  427,  418,    0,    0,
      410,  408,  421,  441,  418,  407,  383,  395,  864,  352,
      413,  423,    0,  323,    0,  315,  297,    0,  445,    0,
      266,  864,  245,  194,  864,  178,  151,  433,  435,  123,
       87,    0,   61,    0,   27,   21,  864,    0,    0,    0,
      455,  864,  864,  864,  466,  477,  488,  498,  501,  512,
      523,  534,  538,  549,  553,  555,  564,  572,  576,  587,
      595,  599,  605
    ) ;

yy_def : constant array(0..573) of short :=
    (   0,
      554,    1,    1,    2,    1,    2,    1,    2,  555,  555,
      556,  556,  555,  555,  554,  554,  554,  554,  554,  557,
      558,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  559,  554,  554,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  554,  554,  554,  554,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  554,  554,  560,   65,  554,
      554,  554,  554,  561,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,

      554,  554,  554,  554,  554,  557,  554,  557,  562,  563,
      554,  554,  554,  554,  564,  554,  554,  554,  554,  554,
      565,  554,  554,  554,  554,  566,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  554,  554,  554,   65,  567,
      554,  554,  554,  554,  554,  559,  559,  559,  559,  559,

      559,  559,  559,  559,  559,  559,  559,  559,  554,  554,
      560,   79,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  562,  563,  554,  564,  554,  568,  554,  554,
      554,  554,  569,  570,  554,  554,  554,  554,  554,  554,
      554,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      554,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  554,  554,  554,  554,  554,

      554,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  554,  554,  571,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  572,  568,  568,  554,  554,  554,  554,
      554,  554,  554,  569,  569,  570,  554,  559,  554,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  554,  554,  554,  554,  559,
      559,  559,  559,  559,  559,  559,  559,  571,  554,  554,

      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  573,  554,  554,  559,  559,  559,  559,  559,  559,
      559,  554,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  554,  559,  559,  559,  559,  559,  559,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      573,  573,  554,  554,  559,  559,  559,  554,  559,  559,
      559,  559,  559,  559,  559,  559,  559,  559,  559,  559,
      559,  559,  559,  554,  559,  559,  559,  559,  559,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  559,  559,

      559,  554,  559,  559,  559,  559,  559,  559,  559,  559,
      554,  559,  559,  559,  559,  554,  554,  554,  554,  554,
      554,  554,  559,  559,  559,  559,  559,  559,  554,  559,
      559,  554,  559,  554,  554,  554,  554,  554,  554,  559,
      559,  559,  559,  559,  554,  554,  554,  559,  559,  559,
      554,  554,  554,    0,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554
    ) ;

yy_nxt : constant array(0..925) of short :=
    (   0,
       16,   17,   18,   17,   19,   20,   21,   16,   22,   23,
       24,   25,   26,   27,   28,   29,   30,   31,   32,   33,
       34,   35,   36,   37,   38,   38,   38,   38,   38,   39,
       16,   40,   16,   16,   41,   42,   43,   44,   45,   46,
       47,   38,   48,   38,   49,   50,   51,   52,   53,   54,
       55,   56,   57,   58,   59,   60,   61,   38,   62,   63,
       64,   65,   79,   65,   79,   81,   81,  109,   78,  109,
      111,   80,  109,  552,   80,  100,  101,  100,  551,   84,
      102,  112,   84,  118,  113,  179,  119,  103,  113,  104,
      220,  114,  107,  221,   66,  107,   67,   68,   69,   70,

      115,  180,   71,  140,  141,  142,   72,   73,  116,  117,
       74,   85,   75,  550,   85,   86,   82,   82,   86,   76,
      108,   87,  187,  107,   87,  143,  144,  109,   88,  151,
      166,   88,   89,  120,   90,   91,   92,   93,  549,  152,
       94,  121,  153,  167,   95,   96,  262,  135,   97,  122,
       98,  123,  123,  136,  137,  263,  149,   99,  121,  159,
      188,  125,  150,  146,  162,  160,  122,  127,  123,  123,
      163,  164,  147,  125,  148,  548,  165,  124,  125,  159,
      126,  128,  129,  128,  127,  202,  130,  124,  169,  547,
      125,  131,  138,  139,  155,  132,  172,  170,  133,  183,

      184,  134,  196,  171,  156,  173,  157,  174,  126,  187,
      151,  147,  212,  148,  212,  155,  158,  189,  101,  189,
      197,  205,  102,  198,  546,  156,  190,  200,  162,  103,
      170,  104,  172,  208,  203,  204,  171,  201,  223,  107,
      165,  206,  107,  174,  226,  227,  545,  209,  224,  180,
      213,  100,  101,  100,  191,  109,  102,  109,  192,  244,
      109,  244,  193,  103,  244,  104,  121,  108,  194,  359,
      107,  239,  239,  360,  122,  195,  123,  123,  240,  240,
      123,  123,  241,  544,  241,  411,  125,  242,  242,  128,
      129,  128,  127,  274,  130,  247,  248,  411,  125,  265,

      284,  265,  333,  132,  275,  266,  285,  307,  312,  267,
      334,  267,  543,  286,  285,  287,  314,  314,  314,  316,
      210,  286,  354,  287,  337,  337,  340,  340,  336,  242,
      242,  342,  342,  368,  338,  542,  369,  355,  356,  392,
      339,  422,  341,  388,  388,  388,  338,  343,  314,  314,
      314,  337,  337,  368,  355,  356,  395,  314,  314,  314,
      401,  401,  401,  333,  554,  337,  337,  541,  413,  540,
      413,  334,  554,  414,  414,  338,  340,  340,  340,  340,
      423,  339,  342,  342,  342,  342,  460,  338,  537,  336,
      554,  460,  341,  314,  314,  314,  414,  414,  343,  449,

      449,  449,  451,  451,  451,  458,  498,  458,  463,  463,
      459,  459,  462,  459,  459,  496,  496,  462,  498,  554,
      463,  463,  463,  463,  464,  496,  496,  496,  496,  522,
      522,  497,  521,  536,  521,  535,  464,  522,  522,  538,
      538,  497,  532,  532,  532,  554,  532,  532,  532,  538,
      538,  538,  538,  534,  533,  539,  553,  553,  553,  531,
      530,  529,  528,  527,  526,  539,   80,   80,   80,   80,
       80,   80,   80,   80,   80,   80,   80,   83,   83,   83,
       83,   83,   83,   83,   83,   83,   83,   83,  106,  106,
      106,  106,  106,  106,  106,  106,  106,  106,  106,  110,

      525,  110,  524,  523,  520,  519,  110,  110,  145,  145,
      145,  145,  210,  210,  210,  210,  518,  210,  210,  210,
      210,  210,  210,  214,  214,  214,  214,  214,  517,  214,
      214,  214,  214,  214,  233,  233,  516,  233,  233,  233,
      233,  233,  233,  233,  233,  234,  234,  234,  234,  236,
      236,  515,  236,  236,  236,  236,  236,  236,  236,  236,
      238,  238,  243,  243,  296,  296,  296,  296,  296,  514,
      296,  296,  296,  296,  296,  335,  513,  512,  335,  335,
      335,  511,  335,  344,  344,  510,  344,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  398,  509,

      508,  507,  398,  398,  506,  398,  412,  412,  461,  505,
      504,  503,  461,  461,  502,  461,  501,  500,  499,  495,
      494,  493,  492,  491,  490,  489,  488,  487,  486,  485,
      484,  483,  482,  481,  480,  479,  478,  477,  476,  475,
      474,  473,  472,  471,  470,  469,  468,  467,  466,  465,
      457,  456,  455,  454,  453,  452,  450,  210,  448,  447,
      446,  445,  444,  443,  301,  442,  441,  440,  439,  438,
      437,  436,  435,  434,  433,  432,  431,  430,  429,  428,
      427,  426,  425,  424,  421,  420,  419,  418,  417,  416,
      415,  129,  554,  345,  410,  409,  408,  407,  406,  405,

      404,  403,  402,  400,  399,  210,  397,  396,  394,  393,
      361,  391,  390,  389,  301,  387,  386,  385,  384,  383,
      382,  381,  380,  379,  378,  377,  376,  375,  374,  373,
      372,  371,  370,  367,  366,  365,  364,  363,  362,  361,
      358,  357,  353,  352,  351,  350,  349,  348,  347,  129,
      345,  332,  331,  330,  329,  328,  327,  326,  325,  324,
      323,  322,  321,  320,  319,  318,  317,  315,  313,  280,
      311,  310,  309,  308,  306,  305,  304,  303,  302,  301,
      300,  299,  298,  297,  295,  294,  293,  292,  291,  290,
      289,  288,  283,  282,  281,  280,  279,  278,  277,  276,

      273,  272,  271,  270,  269,  268,  264,  261,  260,  259,
      258,  257,  256,  255,  254,  253,  252,  251,  250,  249,
      246,  245,  244,  113,  237,  113,  235,  554,  109,  115,
      232,  231,  230,  229,  228,  225,  222,  219,  218,  217,
      216,  215,  211,  207,  199,  186,  185,  182,  181,  178,
      177,  176,  175,  168,  161,  154,  105,  554,   78,   78,
       78,   77,   77,   15,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,

      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554
    ) ;

yy_chk : constant array(0..925) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    8,    2,    8,    9,   10,   21,    8,   21,
       24,   11,   21,  546,   12,   17,   17,   17,  545,   11,
       17,   24,   12,   28,   25,   57,   28,   17,   25,   17,
       90,   25,   20,   90,    2,   20,    2,    2,    2,    2,

       27,   57,    2,   35,   35,   35,    2,    2,   27,   27,
        2,   11,    2,  543,   12,   11,    9,   10,   12,    2,
       20,   11,   63,   20,   12,   36,   36,   29,   11,   43,
       49,   12,   14,   29,   14,   14,   14,   14,  541,   43,
       14,   31,   43,   49,   14,   14,  156,   34,   14,   31,
       14,   31,   31,   34,   34,  156,   42,   14,   30,   46,
       63,   31,   42,   41,   48,   46,   30,   31,   30,   30,
       48,   48,   41,   31,   41,  540,   48,   30,   30,   70,
       30,   32,   32,   32,   30,   70,   32,   30,   51,  537,
       30,   32,   34,   34,   45,   32,   52,   51,   32,   60,

       60,   32,   66,   51,   45,   52,   45,   52,   30,   76,
       67,   66,   79,   66,   79,   69,   45,   65,   65,   65,
       67,   72,   65,   67,  536,   69,   65,   69,   71,   65,
       72,   65,   73,   75,   71,   71,   72,   69,   92,  106,
       71,   73,  106,   73,   94,   94,  534,   76,   92,   75,
       79,  100,  100,  100,   65,  102,  100,  102,   65,  130,
      102,  130,   65,  100,  130,  100,  123,  106,   65,  263,
      106,  122,  122,  263,  123,   65,  123,  123,  124,  124,
      127,  127,  125,  533,  125,  333,  123,  125,  125,  128,
      128,  128,  123,  167,  128,  137,  137,  333,  123,  158,

      177,  201,  238,  128,  167,  158,  177,  201,  207,  158,
      238,  201,  531,  177,  207,  177,  209,  209,  209,  211,
      211,  207,  259,  207,  239,  239,  240,  240,  238,  241,
      241,  242,  242,  271,  239,  527,  271,  259,  259,  304,
      239,  359,  240,  298,  298,  298,  239,  242,  301,  301,
      301,  339,  339,  309,  304,  304,  309,  311,  311,  311,
      319,  319,  319,  335,  336,  337,  337,  526,  338,  524,
      338,  335,  336,  338,  338,  337,  340,  340,  341,  341,
      359,  337,  342,  342,  343,  343,  412,  337,  520,  335,
      336,  461,  340,  397,  397,  397,  413,  413,  342,  399,

      399,  399,  402,  402,  402,  411,  460,  411,  414,  414,
      411,  411,  412,  458,  458,  459,  459,  461,  460,  462,
      463,  463,  464,  464,  414,  496,  496,  497,  497,  521,
      521,  459,  498,  518,  498,  517,  463,  498,  498,  522,
      522,  496,  514,  514,  514,  462,  529,  529,  529,  538,
      538,  539,  539,  516,  515,  522,  551,  551,  551,  513,
      512,  511,  508,  507,  506,  538,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  556,  556,  556,
      556,  556,  556,  556,  556,  556,  556,  556,  557,  557,
      557,  557,  557,  557,  557,  557,  557,  557,  557,  558,

      501,  558,  500,  499,  495,  493,  558,  558,  559,  559,
      559,  559,  560,  560,  560,  560,  492,  560,  560,  560,
      560,  560,  560,  561,  561,  561,  561,  561,  491,  561,
      561,  561,  561,  561,  562,  562,  490,  562,  562,  562,
      562,  562,  562,  562,  562,  563,  563,  563,  563,  564,
      564,  489,  564,  564,  564,  564,  564,  564,  564,  564,
      565,  565,  566,  566,  567,  567,  567,  567,  567,  487,
      567,  567,  567,  567,  567,  568,  486,  485,  568,  568,
      568,  484,  568,  569,  569,  483,  569,  570,  570,  570,
      570,  570,  570,  570,  570,  570,  570,  570,  571,  480,

      479,  476,  571,  571,  474,  571,  572,  572,  573,  472,
      471,  470,  573,  573,  468,  573,  467,  466,  465,  457,
      456,  455,  454,  452,  450,  448,  447,  446,  445,  443,
      442,  439,  438,  437,  436,  435,  434,  433,  432,  431,
      430,  429,  428,  427,  426,  423,  422,  421,  419,  415,
      409,  408,  406,  405,  404,  403,  400,  398,  396,  395,
      393,  392,  391,  390,  389,  387,  384,  383,  379,  378,
      377,  376,  375,  372,  371,  370,  369,  368,  367,  365,
      364,  363,  361,  360,  356,  355,  354,  353,  351,  350,
      348,  346,  345,  344,  332,  329,  328,  327,  326,  323,

      322,  321,  320,  318,  317,  316,  313,  310,  308,  307,
      306,  303,  302,  300,  299,  297,  295,  294,  293,  291,
      290,  289,  288,  287,  286,  283,  282,  280,  279,  275,
      274,  273,  272,  270,  269,  268,  267,  266,  265,  264,
      262,  261,  258,  257,  256,  255,  254,  252,  245,  244,
      243,  231,  230,  228,  227,  226,  225,  224,  223,  222,
      221,  220,  219,  217,  216,  215,  213,  210,  208,  206,
      205,  204,  203,  202,  200,  199,  198,  197,  196,  195,
      194,  193,  192,  191,  185,  184,  183,  182,  181,  180,
      179,  178,  176,  175,  174,  173,  171,  170,  169,  168,

      166,  164,  163,  161,  160,  159,  157,  155,  154,  153,
      152,  151,  150,  149,  148,  147,  146,  144,  140,  138,
      136,  135,  132,  131,  119,  118,  111,  108,  104,  103,
       99,   98,   97,   96,   95,   93,   91,   89,   88,   87,
       86,   85,   78,   74,   68,   62,   61,   59,   58,   56,
       55,   54,   53,   50,   47,   44,   19,   15,    7,    6,
        5,    4,    3,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,

      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554,  554,  554,  554,  554,  554,
      554,  554,  554,  554,  554
    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if (text_io.is_open(user_output_file)) then
     text_io.put( user_output_file, yytext );
   else
     text_io.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
    yy_bp : integer := yytext_ptr;
begin
    yy_current_state := yy_start;
    if ( yy_ch_buf(yy_bp-1) = ASCII.LF ) then
	yy_current_state := yy_current_state + 1;
    end if;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if ( yy_accept(yy_current_state) /= 0 ) then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 555 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(text_io.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters.  The first causes
        -- a transition to the end-of-buffer state.  The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
    end if; -- yy_init

    loop                -- loops until end-of-file is reached


        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	if ( yy_ch_buf(yy_bp-1) = ASCII.LF ) then
	    yy_current_state := yy_current_state + 1;
	end if;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if ( yy_accept(yy_current_state) /= 0 ) then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 555 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 554 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put( Standard_Error, "--accepting rule #" );
            text_io.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            text_io.put_line( Standard_Error, "(""" & yytext & """)");
        end if;


<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;



-- ^[ \r\t\f]+/"queued"[ \r\t\f\n] {
--         -- ignore "queued" at start of line as far as indenting
--         ECHO_L; 
--         if Debug_Indent and then Expecting_Indent then
--            Text_IO.Put(" [queued: indent off] ");
--         end if;
--         Expecting_Indent := False;  -- "then" will turn it back on
-- }
when 1 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 3;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 67 "parasail_lex.l"

        -- ignore "is" at start of line as far as indenting
        ECHO_L; 


when 2 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 72 "parasail_lex.l"

        -- this keyword when used at the beginning of a line
        -- is expected to be "outdented" relative to the preceding and
        -- following construct.  We want to treat it as though it
        -- it is the same indent as the prior construct that is just slightly
        -- more indented than this keyword.
        -- However, if we are expecting an *indent* then we don't look
        -- at the stack, and just return an INDENT, while preserving
        -- the "expecting indent" flag.
        -- If not expecting an indent, then we generate zero or more
        -- OUTDENTs and one NEWLINE if it is outdented relative to
        -- the top of the indent stack.  If not outdented relative to
        -- the top of the indent stack, we don't produce anything extra.

    declare
        YYT : constant String := yytext;
        Exports_Length : constant := 7; --  "exports"'Length
    begin
        pragma Assert (Col_Count = 0);

        --  Start of line, determine indent
        ECHO_L(YYT(YYT'First .. YYT'Last - Exports_Length)); 

        --  Put "exports" back
        for I in reverse YYT'Last - Exports_Length + 1 .. YYT'Last loop
           unput(YYT(I));
        end loop;

        if Col_Count = 0 then
           ENTER(RESCANNING);
        end if;

        --  At this point, Col_Count is indent level

        if Expecting_Indent then
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning INDENT] ");
              Text_IO.Flush;
           end if;
           --  Preserve indent expectation
           Expecting_Indent := True;
           return INDENT;
        elsif Top = 0 then
           --  No indents on stack yet
           --  This shouldn't happen.
           yyerror("Source file cannot start with this token");
        elsif Col_Count >= Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           --  Do nothing
           null;
        elsif Top = 1 then
           --  This is unexpected; should be at least two
           --  Do nothing
           null;
        elsif Col_Count >= Indent_Stack(Top-1) then
           --  See where we stand relative to the next indent level.
           --  We are not more outdented than next level, so return NEWLINE
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           return NEWLINE;
        else
           --  Outdenting relative to remembered indent;
           --  we will set Col_Match_Expected to False so we
           --  won't complain about a col-count mismatch.
           pragma Assert (Col_Count < Indent_Stack (Top-1));

           Col_Match_Expected := False;

           if Debug_Indent then
              Text_IO.Put_Line(" [entering OUTDENTING; returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           ENTER(OUTDENTING);
           return NEWLINE;
        end if;
        if Debug_Indent and then Expecting_Indent then
           Text_IO.Put(" [NL: indent off] ");
        end if;
        Expecting_Indent := False;
    end;


when 3 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 158 "parasail_lex.l"

        -- ignore "new", "then", "||" at start of line as far as indenting
        -- but return a NEWLINE if not expecting an INDENT
    declare
        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        for I in reverse YYT'Range loop
           if YYT(I) <= ' ' then
              --  Echo the spaces at the front of the line
              ECHO_L(YYT(YYT'First .. I));
              exit;
           end if;
           --  Unput the non-space characters
           unput(YYT(I));
        end loop;

        if Col_Count = 0 then
           --  Next time, return the token
           ENTER(RESCANNING);
        end if;

        if not Expecting_Indent and then not Inside_For_Header then
           yylval := Create_Token (Text => "");
           return NEWLINE;
        end if;
    end;


when 4 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 2;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 189 "parasail_lex.l"

        -- Remember Expecting_Indent state, and return INDENT
        -- if expecting indent, but don't push new indent level on stack;
        -- return NEWLINE if not expecting an indent.
        Expecting_Indent_At_Label := Expecting_Indent;
        ECHO_L; 
        if Expecting_Indent then
            yylval := Create_Token(Text => "");
            if Debug_Indent then
               Text_IO.Put_Line(" [label: returning INDENT] "); Text_IO.Flush;
            end if;
            Expecting_Indent := True;
            return INDENT;
        else
            return NEWLINE;
        end if;


when 5 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 4;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 207 "parasail_lex.l"

        -- ignore "for" after "implements" at start of line as far as indenting
        ECHO_L; 
        ENTER(CHARLIT);


when 6 => 
--# line 213 "parasail_lex.l"

    declare
        -- ignore spaces,Carriage returns,tabs,form feeds
        -- 
        -- determine indent (expanding tabs appropriately)
        -- compare to top of indent "stack"
        -- if less, emit OUTDENTs while popping from stack until find match
        --   (except ignore such a line if it starts with 
        --     "then"/"new"/"implements"/"exports"/"||"/"*XXX*")
        --   * complain if new level does not match one already on stack
        -- if same, emit NEWLINE; leave stack as is.
        -- if indent is more than top of stack:
        --   * if prior line ends with ":", push indent on stack and emit INDENT
        --   * if prior line does *not* end with ":" do not push indent on stack

        YYT : constant String := yytext;
    begin

        if Col_Count /= 0 then
           Parser_Warning("Expected column zero, found" &
             Integer'Image(Col_Count));
           Col_Count := 0;
        end if;

        if YYT(YYT'First) in 'a'..'z' then
           --  Indent is zero; put back characters
           for I in reverse YYT'Range loop
              unput(YYT(I));
           end loop;
           --  Next time, return the token
           ENTER(RESCANNING);
        else
           --  Start of line, determine indent
           ECHO_L; 
        end if;

        --  At this point, Col_Count is indent level

        if Top = 0 then
           --  No indents on stack yet
           Top := Top + 1;
           Indent_Stack(Top) := Col_Count;
           Bracketing_Token_Stack(Top) := Error;
           if Debug_Indent then
              Text_IO.Put_Line(" [pushing" & Integer'Image(Col_Count) &
                " on Indent_Stack] ");
              Text_IO.Flush;
           end if;
        elsif Col_Count > Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           if Expecting_Indent then
              --  Indenting is only significant if
              --  preceded by ':'/is/of/then/else/loop
              Top := Top + 1;
              Indent_Stack(Top) := Col_Count;
              Bracketing_Token_Stack(Top) := Bracketing_Token;
              if Debug_Indent then
                 Text_IO.Put_Line(" [pushing" & Integer'Image(Col_Count) &
                   " on Indent_Stack] "); Text_IO.Flush;
              end if;
              yylval := Create_Token(Text => "");
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning INDENT] "); Text_IO.Flush;
              end if;
              return INDENT;
           end if;
        elsif Col_Count = Indent_Stack(Top) then
           --  Same indent, this is a "significant" newline
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           return NEWLINE;
        else
           --  Outdenting relative to remembered indent;
           --  we will pop indent-stack until we get a match
           pragma Assert (Col_Count < Indent_Stack (Top));

           --  tbd: convert last OUTDENT into a NEWLINE: Top := Top - 1;
           if Debug_Indent then
              Text_IO.Put_Line(" [entering OUTDENTING; returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           ENTER(OUTDENTING);
           return NEWLINE;
        end if;
        if Debug_Indent and then Expecting_Indent then
           Text_IO.Put(" [NL: indent off] ");
        end if;
        Expecting_Indent := False;
    end;


when 7 => 
--# line 307 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 8 => 
--# line 313 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 9 => 
--# line 319 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 10 => 
--# line 325 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 11 => 
--# line 331 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 12 => 
--# line 337 "parasail_lex.l"
  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 13 => 
--# line 343 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 14 => 
--# line 349 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 15 => 
--# line 355 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OP_kw);


when 16 => 
--# line 361 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REF_kw);


  -- ParaSail reserved words
when 17 => 
--# line 368 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABS_kw);


when 18 => 
--# line 373 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 19 => 
--# line 378 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALL_kw);


when 20 => 
--# line 383 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AND_kw);


when 21 => 
--# line 388 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BEGIN_kw);


when 22 => 
--# line 393 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := BLOCK_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (BLOCK_kw);


when 23 => 
--# line 403 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CASE_kw);


when 24 => 
--# line 408 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 25 => 
--# line 413 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 26 => 
--# line 418 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONST_kw);


when 27 => 
--# line 423 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONTINUE_kw);


when 28 => 
--# line 428 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EACH_kw);


when 29 => 
--# line 433 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := ELSE_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (ELSE_kw);


when 30 => 
--# line 443 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""elseif""");
	return (ELSIF_kw);


when 31 => 
--# line 449 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
        --  Allow "elif" as an alias for "elsif" for Python-like syntax
	return (ELSIF_kw);


when 32 => 
--# line 455 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""else if""");
	return (ELSIF_kw);


when 33 => 
--# line 461 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ELSIF_kw);


when 34 => 
--# line 466 "parasail_lex.l"

        unput('f'); unput('i');
	ECHO_L("end"); ENTER(CHARLIT); 
        yylval := Create_Token("end");
	yyerror("Use ""end if"" rather than ""endif""");
	return (END_kw);


when 35 => 
--# line 473 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 36 => 
--# line 478 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXIT_kw);


when 37 => 
--# line 483 "parasail_lex.l"

        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (EXPORTS_kw);
        end;


when 38 => 
--# line 496 "parasail_lex.l"

        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (EXPORTS_kw);
        end;


when 39 => 
--# line 509 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXTENDS_kw);


when 40 => 
--# line 514 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FOR_kw);


when 41 => 
--# line 519 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FORWARD_kw);


when 42 => 
--# line 524 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 43 => 
--# line 529 "parasail_lex.l"
  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 44 => 
--# line 534 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GLOBAL_kw);


when 45 => 
--# line 539 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IF_kw);


when 46 => 
--# line 544 "parasail_lex.l"

	ECHO_L; ENTER(AFTER_IMPLEMENTS); 
        --  TBD: We might want to return a NEWLINE first.
	yylval := Create_Token;
	return (IMPLEMENTS_kw);


when 47 => 
--# line 550 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 48 => 
--# line 555 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IN_kw);


when 49 => 
--# line 560 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 50 => 
--# line 565 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := IS_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (IS_kw);


when 51 => 
--# line 575 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LAMBDA_kw);


when 52 => 
--# line 580 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LOCKED_kw);


when 53 => 
--# line 585 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := LOOP_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (LOOP_kw);


when 54 => 
--# line 595 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (MOD_kw);


when 55 => 
--# line 600 "parasail_lex.l"

        declare
           --  Preserve "Expecting_Indent" across "new"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (NEW_kw);
        end;


when 56 => 
--# line 614 "parasail_lex.l"

        declare
           --  Preserve "Expecting_Indent" across "new"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (NEW_kw);
        end;


when 57 => 
--# line 628 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NOT_kw);


when 58 => 
--# line 633 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NULL_kw);


when 59 => 
--# line 638 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := OF_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (OF_kw);


when 60 => 
--# line 648 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OP_kw);


when 61 => 
--# line 653 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPT_kw);


when 62 => 
--# line 658 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPTIONAL_kw);


when 63 => 
--# line 663 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OR_kw);


when 64 => 
--# line 668 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRIVATE_kw);


when 65 => 
--# line 673 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (QUEUED_kw);


when 66 => 
--# line 678 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REF_kw);


when 67 => 
--# line 683 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REM_kw);


when 68 => 
--# line 688 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RETURN_kw);


when 69 => 
--# line 693 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REVERSE_kw);


when 70 => 
--# line 698 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SOME_kw);


when 71 => 
--# line 703 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 72 => 
--# line 713 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 73 => 
--# line 723 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TYPE_kw);


when 74 => 
--# line 728 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (UNTIL_kw);


when 75 => 
--# line 733 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VAR_kw);


when 76 => 
--# line 738 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WHILE_kw);


when 77 => 
--# line 743 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WITH_kw);


when 78 => 
--# line 748 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (XOR_kw);


  -- Match all the compound ParaSail delimiters. 
when 79 => 
--# line 755 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMPARE);


when 80 => 
--# line 760 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(EQ);


when 81 => 
--# line 765 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(NEQ);


when 82 => 
--# line 770 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GEQ);


when 83 => 
--# line 775 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LEQ);


when 84 => 
--# line 780 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT);


when 85 => 
--# line 785 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER);


when 86 => 
--# line 790 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(ASSIGN);


when 87 => 
--# line 795 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MOVE);


when 88 => 
--# line 800 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(SWAP);


when 89 => 
--# line 805 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOT_DOT);


when 90 => 
--# line 810 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_INTERVAL);


when 91 => 
--# line 815 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_CLOSED_INTERVAL);


when 92 => 
--# line 820 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(CLOSED_OPEN_INTERVAL);


when 93 => 
--# line 825 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOUBLE_COLON);


when 94 => 
--# line 830 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);


when 95 => 
--# line 835 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GIVES);


when 96 => 
--# line 840 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(IMPLIES);


when 97 => 
--# line 845 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 98 => 
--# line 850 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 99 => 
--# line 855 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_ASSIGN);


when 100 => 
--# line 860 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_ASSIGN);


when 101 => 
--# line 865 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(TIMES_ASSIGN);


when 102 => 
--# line 870 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DIVIDE_ASSIGN);


when 103 => 
--# line 875 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER_ASSIGN);


when 104 => 
--# line 880 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_ASSIGN);


when 105 => 
--# line 885 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_MOVE);


when 106 => 
--# line 890 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_ASSIGN);


when 107 => 
--# line 895 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_ASSIGN);


when 108 => 
--# line 900 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(XOR_ASSIGN);


when 109 => 
--# line 905 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT_ASSIGN);


when 110 => 
--# line 910 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(RSHIFT_ASSIGN);


when 111 => 
--# line 915 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_BASED_OP);


  -- Match all the ParaSail single-character delimiters.
when 112 => 
--# line 922 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(PRIME);


when 113 => 
--# line 927 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
        Paren_Count := Paren_Count + 1;
        --  Push the paren count after bumping it
        Unquote_Top := Unquote_Top + 1;
        Unquote_Stack (Unquote_Top) := Paren_Count;
	yylval := Create_Token;
	return('(');


when 114 => 
--# line 936 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
        Paren_Count := Paren_Count + 1;
	yylval := Create_Token;
	return('(');


when 115 => 
--# line 942 "parasail_lex.l"

	ECHO_L; ENTER(TICK); 
        if Unquote_Top > 0
          and then Unquote_Stack (Unquote_Top) = Paren_Count
        then
           --  convert end of unquoted expression
           --  into "|" followed by double-quote
           unput (Double_Quote_Char);
           unput (Concat_Char);

           --  Pop the unquote nesting stack
           Unquote_Top := Unquote_Top - 1;
        end if;
        --  Decrement depth of paren nesting
        Paren_Count := Paren_Count - 1;
	yylval := Create_Token (Text => ")");
	return(')');


when 116 => 
--# line 960 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('[');


when 117 => 
--# line 965 "parasail_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(']');


when 118 => 
--# line 970 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('<');


when 119 => 
--# line 975 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('>');


when 120 => 
--# line 980 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);  --  or L_SET in PARython mode


when 121 => 
--# line 985 "parasail_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);


when 122 => 
--# line 990 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);  --  or R_SET in PARython mode


when 123 => 
--# line 995 "parasail_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);


when 124 => 
--# line 1000 "parasail_lex.l"

        declare
           --  Remember Expecting_Indent
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           if Expecting_Indent_At_Label then
              if not Old_Indent then
                 --  Seen '*' twice; set Expecting_Indent back on
                 Expecting_Indent_At_Label := False;
                 Expecting_Indent := True;
                 if Debug_Indent then
                    Text_IO.Put(" [indent on] ");
                 end if;
              end if;
           end if;
	   return ('*');
        end;


when 125 => 
--# line 1020 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('+');


when 126 => 
--# line 1025 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(',');


when 127 => 
--# line 1030 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('-');


when 128 => 
--# line 1035 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('.');


when 129 => 
--# line 1040 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('/');


when 130 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1045 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
        Bracketing_Token := ':';
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(EOL_COLON);


when 131 => 
--# line 1055 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(':');


when 132 => 
--# line 1060 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(';');


when 133 => 
--# line 1065 "parasail_lex.l"

	ECHO_L;
        --  Stay in AFTER_UNQUOTE start state
	yylval := Create_Token;
	return('|');


when 134 => 
--# line 1071 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('|');


when 135 => 
--# line 1076 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('?');


when 136 => 
--# line 1081 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');


when 137 => 
--# line 1087 "parasail_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);


  -- Enumeration literals
when 138 => 
--# line 1094 "parasail_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Enum_Literal);


  -- Decimal numeric literals
when 139 => 
--# line 1101 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 140 => 
--# line 1107 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


  -- Based numeric literals.
when 141 => 
--# line 1115 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 142 => 
--# line 1121 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


when 143 => 
--# line 1127 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


when 144 => 
--# line 1133 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


  -- Match all valid character literals.  See Ada LRM 2.6.
when 145 => 
--# line 1141 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Char_Literal);


  -- Match all valid string literals.  See Ada LRM 2.6.
when 146 => 
--# line 1149 "parasail_lex.l"

	ENTER(CHARLIT); 
        declare
           Tok : String := yytext;
           Tok_Last : Natural := Tok'Last;
        begin
           if Tok'Last > Tok'First and then Tok (Tok'Last) = Unquote_Char
             and then Tok (Tok'Last - 1) /= Back_Slash
           then
              --  An "unquote" character at the end of a string
              --  becomes a (closing) double-quote followed by a concatenate.
              Tok (Tok_Last) := Double_Quote_Char;
              --  Put back a concatenate
              unput (Concat_Char);
              --  Enter special start state
              ENTER(AFTER_UNQUOTE);
           end if;
           ECHO_L (Tok (Tok'First .. Tok_Last));
           yylval := Create_Token (Tok (Tok'First .. Tok_Last));
           if Tok (Tok_Last) /= Double_Quote_Char then
              yyerror ("unterminated string", At_Token => yylval);
           end if;
           return(String_Literal);
        end;


  -- Handle white space --
when 147 => 
--# line 1177 "parasail_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 148 => 
--# line 1179 "parasail_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 149 => 
--# line 1181 "parasail_lex.l"

        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;


when 150 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1188 "parasail_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 151 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1198 "parasail_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 152 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1208 "parasail_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 153 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1218 "parasail_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 154 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1228 "parasail_lex.l"

        --  Ignore outdent for label
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 155 => 
--# line 1238 "parasail_lex.l"

        --  Return another OUTDENT
        declare
           Do_Match_Check : Boolean := True;
        begin
           unput(yytext(1));
           if Top > 1 and then Col_Count < Indent_Stack(Top)
             and then (Col_Match_Expected
               or else Col_Count < Indent_Stack(Top-1))
           then
              --  Not yet reached a matching indent;
              --  Pop the stack
              if Debug_Indent then
                 Text_IO.Put_Line(" [popping" &
                   Integer'Image(Indent_Stack(Top)) &
                   " from Indent_Stack] ");
                 Text_IO.Flush;
              end if;
              Top := Top - 1;
              if Col_Count >= Indent_Stack(Top)
                or else not Col_Match_Expected
                or else Bracketing_Token_Stack(Top + 1) = ':'
                or else Bracketing_Token_Stack(Top + 1) = REFERS_TO
              then
                 --  Don't check for indent match this time;
                 --  wait til next time since only one outdent occurring,
                 --  or ":" or "=>" used at end of line (which allows multiple
                 --  outdents at a time).
                 Do_Match_Check := False;
              end if;
           end if;

           if Do_Match_Check then
              --  Found (nearly) matching indent
              if Debug_Indent then
                 Text_IO.Put_Line(" [exiting OUTDENTING] ");
                 Text_IO.Flush;
              end if;
              if Col_Count = 0 then
                 --  Return the token at the beginning of the line
                 ENTER(RESCANNING);
              else
                 ENTER(CHARLIT);
              end if;
              if Col_Count /= Indent_Stack(Top)
                and then Col_Match_Expected
              then
                 --  Not quite matching
                 Parser_Warning("indent mismatch; expected indent of" &
                   Integer'Image(Indent_Stack(Top)) & ", found" &
                   Integer'Image(Col_Count));

                 --  Force a match
                 --  TBD: Not sure this is the right choice
                 if Top = 1 then
                    Top := Top + 1;
                    if Debug_Indent then
                       Text_IO.Put_Line(" [pushing" &
                         Integer'Image(Col_Count) &
                         " on Indent_Stack] ");
                       Text_IO.Flush;
                    end if;
                 else
                    if Debug_Indent then
                       Text_IO.Put_Line(" [setting top of Indent_Stack to" &
                         Integer'Image(Col_Count) & "] "); Text_IO.Flush;
                    end if;
                 end if;
                 Indent_Stack(Top) := Col_Count;
              end if;
           end if;
           yylval := Create_Token(Text => "");
           if Do_Match_Check then
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning NEWLINE after OUTDENTs] ");
                 Text_IO.Flush;
              end if;
              return NEWLINE;
           else
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning OUTDENT] ");
                 Text_IO.Flush;
              end if;
              return OUTDENT;
           end if;
        end;


when YY_END_OF_BUFFER +CHARLIT + 1 
 |
YY_END_OF_BUFFER +TICK + 1 
 |
YY_END_OF_BUFFER +AFTER_IMPLEMENTS + 1 
 |
YY_END_OF_BUFFER +INITIAL + 1 
 =>
--# line 1326 "parasail_lex.l"

        if Top = 0 then
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [EOF: indent off] "); Text_IO.Flush;
           end if;
           Expecting_Indent := False;
           if Debug_Indent then
              Text_IO.Put(" [returning EOF] "); Text_IO.Flush;
           end if;
           return End_Of_Input;
        else
           unput(YY_END_OF_BUFFER_CHAR);
           Col_Count := Indent_Stack(1);
           if Debug_Indent then
              Text_IO.Put_Line(" [popping" & Integer'Image(Indent_Stack(Top)) &
                " from Indent_Stack] ");
              Text_IO.Flush;
           end if;
           Top := Top - 1;
           if Top > 0 then
              if Debug_Indent then
                 Text_IO.Put_Line(" [entering EOF OUTDENTING] ");
                 Text_IO.Flush;
              end if;
              ENTER(OUTDENTING);
           else
              --  This is a significant NEWLINE
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning NEWLINE] ");
                 Text_IO.Flush;
              end if;
              yylval := Create_Token(Text => "");
              return NEWLINE;
           end if;
        end if;


when YY_END_OF_BUFFER +OUTDENTING + 1 
 =>
--# line 1363 "parasail_lex.l"

        --  Return another OUTDENT
        unput(YY_END_OF_BUFFER_CHAR);
        if Debug_Indent then
           Text_IO.Put_Line(" [popping" & Integer'Image(Indent_Stack(Top)) &
             " from Indent_Stack] ");
           Text_IO.Flush;
        end if;
        Top := Top - 1;
        if Top = 0 then
           --  All done
           if Debug_Indent then
              Text_IO.Put_Line(" [exiting EOF OUTDENTING] ");
              Text_IO.Flush;
           end if;
           ENTER(CHARLIT);
        end if;

        yylval := Create_Token(Text => "");
        if Debug_Indent then
           Text_IO.Put_Line(" [returning EOF OUTDENT] ");
           Text_IO.Flush;
        end if;
        return OUTDENT;


  -- The following matches all new lines (and the preceding white space).
when 158 => 
--# line 1392 "parasail_lex.l"

        --  TBD: Should we return NEWLINE if this line is entirely blank?
        if Echo_Input then
           text_io.new_line;
        end if;
        Display_Linenum;


  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.
when 159 => 
--# line 1403 "parasail_lex.l"
ECHO_L; 
	    yyerror("lexical error: '" &
	      parasail_lex_dfa.yytext & "'");


when 160 => 
--# line 1408 "parasail_lex.l"
raise AFLEX_SCANNER_JAMMED;
when YY_END_OF_BUFFER + AFTER_UNQUOTE + 1 |
YY_END_OF_BUFFER + RESCANNING + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 1408 "parasail_lex.l"

end parasail_lex;


