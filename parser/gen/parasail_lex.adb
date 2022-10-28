
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
YY_END_OF_BUFFER : constant := 160;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
TICK : constant := 1;
CHARLIT : constant := 2;
AFTER_IMPLEMENTS : constant := 3;
AFTER_UNQUOTE : constant := 4;
OUTDENTING : constant := 5;
RESCANNING : constant := 6;
yy_accept : constant array(0..546) of short :=
    (   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  160,  158,  148,  157,  158,  158,
      158,  113,  114,  123,  124,  125,  126,  127,  128,  138,
      138,  130,  131,  117,  135,  118,  134,  136,  115,  116,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  119,  133,  121,    6,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  133,  111,  158,    6,  159,  112,
      132,  154,  154,  154,  154,  154,  154,  159,  159,  159,
      159,  159,  159,  159,  159,  159,  159,  148,  157,    0,

        0,    0,   80,    0,  145,    0,  146,  137,   84,  100,
      110,   98,  147,   99,   94,  110,   88,  101,    0,    0,
      138,    0,    0,    0,    0,    0,  129,    0,  110,    0,
       92,   85,    0,   83,   82,    0,  122,   79,   93,   78,
       81,    0,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,   44,
      136,   47,   49,  136,  136,  136,  136,  136,  136,   58,
       59,   62,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  120,  103,   97,    6,    0,    0,    0,
        0,    0,    0,  136,  136,  136,  136,  136,  136,  136,

      136,   47,  136,    6,  136,   97,    0,    0,    6,    0,
      153,    0,    0,    0,  152,    0,    0,    0,    0,    0,
        0,    0,    0,    0,    0,   15,    0,   96,  146,  137,
      102,  147,   91,    0,  139,  143,    0,  138,  142,    0,
       90,  108,   86,   87,  104,   95,  109,   16,   18,   19,
      136,  136,  136,  136,  136,   42,  136,  136,  136,   34,
      136,  136,  136,   39,  136,  136,  136,  136,  136,  136,
      136,   53,   55,   56,  136,   60,  106,  136,  136,   65,
       66,  136,  136,  136,  136,  136,  136,   74,  136,  136,
       77,    4,    0,    0,    0,    0,    0,   16,  136,  136,

        6,    6,  136,  136,  136,  136,   55,  136,    3,  144,
        0,    0,    0,    0,    0,    0,    0,   12,   10,    0,
        0,    0,    0,   54,    0,  140,    0,    0,    0,  139,
        0,    0,  143,    0,  138,    0,  142,    0,    0,   89,
      136,  105,  136,  136,   22,  136,  136,  136,  136,   27,
       30,   28,  136,  136,   35,  136,  136,  136,   41,  136,
      136,  136,  136,  136,  136,   52,   57,  136,  136,  136,
      136,  136,   69,   71,   72,  136,  136,   76,  107,    0,
        1,    0,  136,  136,  136,  136,    6,  136,  136,   71,
        0,    0,  151,    0,    0,    0,    0,    0,   11,    0,

        0,   70,    0,    0,    0,  139,  136,   20,   21,   23,
      136,   25,  136,    0,  136,   32,   33,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,   73,   75,    0,  136,    6,  136,  136,  136,  136,
        5,    0,  149,    0,    8,    0,    0,    0,    0,    0,
      140,  141,    0,    0,  139,    0,  136,  136,  136,    0,
       29,  136,  136,  136,   43,  136,   46,  136,   50,   51,
      136,  136,   64,   67,  136,    0,  136,  136,  136,    6,
      136,    0,    0,    0,    0,   13,    0,  140,    0,    0,
      136,  136,  136,   31,   37,   38,   40,  136,  136,  136,

       63,   68,    0,  136,  136,   37,  136,    0,    0,    0,
       36,    0,    0,  141,   17,  136,   26,  136,  136,   61,
        0,    6,  136,    2,  136,    0,    7,    0,    0,  141,
        0,  136,  136,   48,  136,    6,    0,    0,   14,   24,
       45,    6,    0,    9,  150,    0
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

yy_base : constant array(0..564) of short :=
    (   0,
        0,   60,  852,  119,  851,  850,  849,   61,   57,   58,
       69,   72,    0,  145,  856,  861,   74,  861,  833,   99,
       66,  861,  861,   60,   70,  861,   87,   72,  112,  199,
      182,  133,  861,  126,  120,   71,  861,    0,  861,  861,
      124,  125,  128,  815,  156,  120,  808,  172,   56,  804,
      142,  171,  800,  796,  810,  800,  119,  800,  811,   96,
      797,  821,   64,  861,  255,  194,  130,  804,  198,  154,
      200,  156,  202,  165,  205,  861,  811,  227,  861,  861,
      861,  861,    0,  795,  801,  797,  778,  801,   66,  797,
       49,  781,  104,  795,  784,  790,  771,  270,  861,  274,

      816,  813,  861,  257,  861,  825,    0,    0,  805,  861,
      861,  861,    0,  861,  861,  811,  804,  861,    0,  219,
      272,  231,  283,    0,  265,  310,  861,  302,  805,  807,
      861,  861,  807,  799,  270,  798,  861,  796,  861,  861,
      861,  796,    0,  765,  771,  777,  773,  765,  760,  776,
      763,  769,  771,  218,  769,  201,  755,  758,  756,    0,
      754,  749,    0,  755,  227,  762,  743,  745,  752,    0,
      743,  773,  751,  754,  270,  746,  752,  741,  736,  737,
      744,  733,  734,  861,  861,  861,    0,    0,  727,  731,
      743,  739,  720,  727,  743,  730,  736,  737,  275,  727,

      724,  719,  715,  717,  730,  317,  760,   71,    0,  719,
      861,  717,  709,  725,  861,  711,  727,  714,  720,  721,
      709,  710,  707,  702,  698,  861,  714,  861,    0,    0,
      861,    0,  861,  315,  314,  316,  318,  321,  719,  748,
      729,  861,  861,  861,  861,  861,  861,  696,    0,  726,
      704,  709,  706,  692,  290,    0,  701,  702,  264,  698,
      687,  691,  699,  681,  699,  699,  292,  695,  697,  688,
      682,    0,    0,    0,  685,  686,  861,  673,  673,    0,
        0,  672,  686,  685,  676,  683,  678,    0,  675,  677,
      696,  861,  668,  342,  660,  676,  348,  661,  661,  318,

        0,  669,  663,  673,  311,  670,  358,  661,  861,  861,
      656,  661,  361,  658,  651,  651,  665,  861,  861,  653,
      663,  651,  659,  861,  650,  241,    0,  351,  360,  355,
      362,  351,  368,  360,  372,  374,  663,  662,  691,  861,
      642,  861,  645,  647,    0,  638,  635,  635,  644,    0,
        0,  353,  646,  645,    0,  633,  636,  647,    0,  646,
      641,  628,  627,  639,  637,    0,    0,  627,  639,  634,
      621,  620,    0,    0,    0,  625,  630,    0,  861,  620,
      861,  620,  615,  613,  610,  612,    0,  611,  610,  395,
      400,  621,  861,  404,  608,  606,  603,  605,  861,  604,

      603,  861,  397,  380,  399,  401,  618,    0,    0,    0,
      601,    0,  604,  607,  609,    0,    0,  595,  609,  595,
      600,  598,  590,  602,  606,  602,  592,  585,  599,  589,
      583,    0,    0,  583,  598,    0,  581,  578,  577,  589,
      861,  582,  861,  592,  861,  575,  572,  571,  583,  403,
      405,  356,  393,  403,  407,  410,  585,  570,  566,  579,
        0,  566,  565,  578,    0,  576,    0,  578,    0,    0,
      575,  570,    0,    0,  569,  539,  551,  533,  531,    0,
      541,  519,  506,  484,  471,  861,  477,  412,  414,  425,
      458,  471,  470,  861,    0,    0,    0,  460,  434,  425,

        0,    0,  417,  415,  427,  444,  428,  417,  410,  405,
      861,  404,  432,  434,    0,  388,    0,  380,  373,    0,
      451,    0,  334,  861,  290,  243,  861,  238,  195,  439,
      441,  141,  101,    0,   93,    0,   79,   73,  861,    0,
        0,    0,  458,  861,  861,  861,  472,  483,  494,  504,
      507,  518,  529,  540,  544,  555,  559,  561,  570,  578,
      582,  593,  597,  603
    ) ;

yy_def : constant array(0..564) of short :=
    (   0,
      546,    1,    1,    1,    1,    4,    1,    4,  547,  547,
      548,  548,  547,  547,  546,  546,  546,  546,  546,  549,
      550,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  551,  546,  546,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  546,  546,  546,  546,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  546,  546,  552,   65,  546,  546,
      546,  546,  553,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,

      546,  546,  546,  549,  546,  549,  554,  555,  546,  546,
      546,  546,  556,  546,  546,  546,  546,  546,  557,  546,
      546,  546,  546,  558,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  546,  546,  546,   65,  559,  546,  546,
      546,  546,  546,  551,  551,  551,  551,  551,  551,  551,

      551,  551,  551,  551,  551,  546,  546,  552,   78,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  554,  555,
      546,  556,  546,  560,  546,  546,  546,  546,  561,  562,
      546,  546,  546,  546,  546,  546,  546,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  546,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  546,  546,  546,  546,  546,  546,  551,  551,  551,

      551,  551,  551,  551,  551,  551,  551,  551,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  563,  560,  560,  546,
      546,  546,  546,  546,  546,  546,  561,  561,  562,  546,
      551,  546,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  546,  546,
      546,  546,  551,  551,  551,  551,  551,  551,  551,  551,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,

      546,  546,  546,  564,  546,  546,  551,  551,  551,  551,
      551,  551,  551,  546,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  546,  551,  551,  551,  551,  551,  551,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  564,  564,  546,  546,  551,  551,  551,  546,
      551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
      551,  551,  551,  551,  551,  546,  551,  551,  551,  551,
      551,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      551,  551,  551,  546,  551,  551,  551,  551,  551,  551,

      551,  551,  546,  551,  551,  551,  551,  546,  546,  546,
      546,  546,  546,  546,  551,  551,  551,  551,  551,  551,
      546,  551,  551,  546,  551,  546,  546,  546,  546,  546,
      546,  551,  551,  551,  551,  551,  546,  546,  546,  551,
      551,  551,  546,  546,  546,    0,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546
    ) ;

yy_nxt : constant array(0..922) of short :=
    (   0,
       16,   17,   18,   17,   19,   20,   21,   16,   22,   23,
       24,   25,   26,   27,   28,   29,   30,   31,   32,   33,
       34,   35,   36,   37,   38,   38,   38,   38,   38,   39,
       16,   40,   16,   16,   41,   42,   43,   44,   45,   46,
       47,   38,   48,   38,   49,   50,   51,   52,   53,   54,
       55,   56,   57,   58,   59,   60,   61,   38,   62,   63,
       64,   65,   78,   65,   78,   80,   80,  107,   77,  107,
      109,   79,  107,  546,   79,   98,   99,   98,  207,   83,
      100,  110,   83,  116,  111,  185,  117,  101,  111,  102,
      164,  112,  141,  142,   66,  220,   67,   68,   69,   70,

      113,  105,   71,  165,  105,  221,   72,   73,  114,  115,
      217,   84,   74,  218,   84,   85,   81,   81,   85,   75,
       65,   86,   65,  186,   86,  544,   76,  107,   87,  106,
      543,   87,  105,  118,  126,  127,  126,  181,  182,  128,
      133,  138,  139,  140,  129,  542,  134,  135,  130,  223,
      224,  131,  541,   66,  132,   67,   68,   69,   70,  144,
      177,   71,  149,  147,  149,   72,   73,  157,  145,  148,
      146,   74,  150,  158,  195,  151,  178,  196,   75,   88,
      167,   89,   90,   91,   92,  136,  137,   93,  119,  168,
      153,   94,   95,  540,  203,  169,  120,   96,  121,  121,

      154,  157,  155,  168,   97,  119,  205,  200,  123,  169,
      170,  160,  156,  120,  125,  121,  121,  161,  162,  171,
      123,  172,  178,  163,  122,  123,  185,  124,  209,  194,
      209,  125,  153,  539,  122,  235,  235,  123,  145,  160,
      146,  170,  154,  261,  198,  201,  202,  236,  236,  262,
      204,  163,  172,  263,  199,  124,  187,   99,  187,  105,
      258,  100,  105,  270,  206,  188,  210,  403,  101,  259,
      102,   98,   99,   98,  271,  107,  100,  107,  119,  403,
      107,  121,  121,  101,  538,  102,  120,  106,  121,  121,
      105,  243,  244,  189,  237,  537,  237,  190,  123,  238,

      238,  191,  352,  240,  125,  240,  353,  192,  240,  280,
      123,  126,  127,  126,  193,  281,  128,  261,  309,  309,
      309,  326,  282,  303,  283,  130,  347,  263,  536,  327,
      330,  330,  333,  333,  238,  238,  361,  335,  335,  362,
      331,  348,  349,  381,  381,  381,  332,  329,  334,  309,
      309,  309,  331,  336,  385,  361,  414,  326,  388,  309,
      309,  309,  393,  393,  393,  327,  546,  330,  330,  348,
      349,  330,  330,  405,  546,  405,  333,  333,  406,  406,
      535,  331,  490,  329,  333,  333,  452,  332,  335,  335,
      335,  335,  546,  331,  490,  415,  309,  309,  309,  452,

      334,  441,  441,  441,  336,  443,  443,  443,  450,  546,
      450,  534,  454,  451,  451,  406,  406,  455,  455,  451,
      451,  488,  488,  455,  455,  454,  455,  455,  488,  488,
      488,  488,  533,  456,  532,  546,  513,  489,  513,  456,
      529,  514,  514,  528,  489,  524,  524,  524,  514,  514,
      530,  530,  524,  524,  524,  530,  530,  530,  530,  545,
      545,  545,  527,  526,  525,  523,  531,  522,  521,  520,
      519,  531,   79,   79,   79,   79,   79,   79,   79,   79,
       79,   79,   79,   82,   82,   82,   82,   82,   82,   82,
       82,   82,   82,   82,  104,  104,  104,  104,  104,  104,

      104,  104,  104,  104,  104,  108,  518,  108,  517,  516,
      515,  512,  108,  108,  143,  143,  143,  143,  207,  207,
      207,  207,  511,  207,  207,  207,  207,  207,  207,  211,
      211,  211,  211,  211,  510,  211,  211,  211,  211,  211,
      229,  229,  509,  229,  229,  229,  229,  229,  229,  229,
      229,  230,  230,  230,  230,  232,  232,  508,  232,  232,
      232,  232,  232,  232,  232,  232,  234,  234,  239,  239,
      292,  292,  292,  292,  292,  507,  292,  292,  292,  292,
      292,  328,  506,  505,  328,  328,  328,  504,  328,  337,
      337,  503,  337,  339,  339,  339,  339,  339,  339,  339,

      339,  339,  339,  339,  404,  404,  453,  502,  501,  500,
      453,  453,  499,  453,  498,  497,  496,  495,  494,  493,
      492,  491,  487,  486,  485,  484,  483,  482,  481,  480,
      479,  478,  477,  476,  475,  474,  473,  472,  471,  470,
      469,  468,  467,  466,  465,  464,  463,  462,  461,  460,
      459,  458,  457,  449,  448,  447,  446,  445,  444,  442,
      440,  439,  438,  437,  436,  435,  297,  434,  433,  432,
      431,  430,  429,  428,  427,  426,  425,  424,  423,  422,
      421,  420,  419,  418,  417,  416,  413,  412,  411,  410,
      409,  408,  407,  127,  546,  338,  402,  401,  400,  399,

      398,  397,  396,  395,  394,  392,  391,  390,  389,  387,
      386,  354,  384,  383,  382,  297,  380,  379,  378,  377,
      376,  375,  374,  373,  372,  371,  370,  369,  368,  367,
      366,  365,  364,  363,  360,  359,  358,  357,  356,  355,
      354,  351,  350,  346,  345,  344,  343,  342,  341,  340,
      127,  338,  325,  324,  323,  322,  321,  320,  319,  318,
      317,  316,  315,  314,  313,  312,  311,  310,  308,  276,
      307,  306,  305,  304,  302,  301,  300,  299,  298,  297,
      296,  295,  294,  293,  291,  290,  289,  288,  287,  286,
      285,  284,  279,  278,  277,  276,  275,  274,  273,  272,

      269,  268,  267,  266,  265,  264,  260,  257,  256,  255,
      254,  253,  252,  251,  250,  249,  248,  247,  246,  245,
      242,  241,  240,  111,  233,  111,  231,  546,  107,  113,
      228,  227,  226,  225,  222,  219,  216,  215,  214,  213,
      212,  208,  197,  184,  183,  180,  179,  176,  175,  174,
      173,  166,  159,  152,  103,  546,   77,   77,   77,   76,
       15,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,

      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546
    ) ;

yy_chk : constant array(0..922) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    8,    2,    8,    9,   10,   21,    8,   21,
       24,   11,   21,  208,   12,   17,   17,   17,  208,   11,
       17,   24,   12,   28,   25,   63,   28,   17,   25,   17,
       49,   25,   36,   36,    2,   91,    2,    2,    2,    2,

       27,   20,    2,   49,   20,   91,    2,    2,   27,   27,
       89,   11,    2,   89,   12,   11,    9,   10,   12,    2,
        4,   11,    4,   63,   12,  538,    4,   29,   11,   20,
      537,   12,   20,   29,   32,   32,   32,   60,   60,   32,
       34,   35,   35,   35,   32,  535,   34,   34,   32,   93,
       93,   32,  533,    4,   32,    4,    4,    4,    4,   41,
       57,    4,   43,   42,   67,    4,    4,   46,   41,   42,
       41,    4,   43,   46,   67,   43,   57,   67,    4,   14,
       51,   14,   14,   14,   14,   34,   34,   14,   31,   51,
       45,   14,   14,  532,   72,   51,   31,   14,   31,   31,

       45,   70,   45,   72,   14,   30,   74,   70,   31,   72,
       52,   48,   45,   30,   31,   30,   30,   48,   48,   52,
       31,   52,   74,   48,   30,   30,   75,   30,   78,   66,
       78,   30,   69,  529,   30,  120,  120,   30,   66,   71,
       66,   73,   69,  156,   69,   71,   71,  122,  122,  156,
       73,   71,   73,  156,   69,   30,   65,   65,   65,  104,
      154,   65,  104,  165,   75,   65,   78,  326,   65,  154,
       65,   98,   98,   98,  165,  100,   98,  100,  121,  326,
      100,  125,  125,   98,  528,   98,  121,  104,  121,  121,
      104,  135,  135,   65,  123,  526,  123,   65,  121,  123,

      123,   65,  259,  128,  121,  128,  259,   65,  128,  175,
      121,  126,  126,  126,   65,  175,  126,  199,  206,  206,
      206,  234,  175,  199,  175,  126,  255,  199,  525,  234,
      235,  235,  236,  236,  237,  237,  267,  238,  238,  267,
      235,  255,  255,  294,  294,  294,  235,  234,  236,  297,
      297,  297,  235,  238,  300,  305,  352,  328,  305,  307,
      307,  307,  313,  313,  313,  328,  329,  332,  332,  300,
      300,  330,  330,  331,  329,  331,  334,  334,  331,  331,
      523,  330,  452,  328,  333,  333,  404,  330,  335,  335,
      336,  336,  329,  330,  452,  352,  390,  390,  390,  453,

      333,  391,  391,  391,  335,  394,  394,  394,  403,  454,
      403,  519,  404,  403,  403,  405,  405,  406,  406,  450,
      450,  451,  451,  455,  455,  453,  456,  456,  488,  488,
      489,  489,  518,  406,  516,  454,  490,  451,  490,  455,
      512,  490,  490,  510,  488,  506,  506,  506,  513,  513,
      514,  514,  521,  521,  521,  530,  530,  531,  531,  543,
      543,  543,  509,  508,  507,  505,  514,  504,  503,  500,
      499,  530,  547,  547,  547,  547,  547,  547,  547,  547,
      547,  547,  547,  548,  548,  548,  548,  548,  548,  548,
      548,  548,  548,  548,  549,  549,  549,  549,  549,  549,

      549,  549,  549,  549,  549,  550,  498,  550,  493,  492,
      491,  487,  550,  550,  551,  551,  551,  551,  552,  552,
      552,  552,  485,  552,  552,  552,  552,  552,  552,  553,
      553,  553,  553,  553,  484,  553,  553,  553,  553,  553,
      554,  554,  483,  554,  554,  554,  554,  554,  554,  554,
      554,  555,  555,  555,  555,  556,  556,  482,  556,  556,
      556,  556,  556,  556,  556,  556,  557,  557,  558,  558,
      559,  559,  559,  559,  559,  481,  559,  559,  559,  559,
      559,  560,  479,  478,  560,  560,  560,  477,  560,  561,
      561,  476,  561,  562,  562,  562,  562,  562,  562,  562,

      562,  562,  562,  562,  563,  563,  564,  475,  472,  471,
      564,  564,  468,  564,  466,  464,  463,  462,  460,  459,
      458,  457,  449,  448,  447,  446,  444,  442,  440,  439,
      438,  437,  435,  434,  431,  430,  429,  428,  427,  426,
      425,  424,  423,  422,  421,  420,  419,  418,  415,  414,
      413,  411,  407,  401,  400,  398,  397,  396,  395,  392,
      389,  388,  386,  385,  384,  383,  382,  380,  377,  376,
      372,  371,  370,  369,  368,  365,  364,  363,  362,  361,
      360,  358,  357,  356,  354,  353,  349,  348,  347,  346,
      344,  343,  341,  339,  338,  337,  325,  323,  322,  321,

      320,  317,  316,  315,  314,  312,  311,  308,  306,  304,
      303,  302,  299,  298,  296,  295,  293,  291,  290,  289,
      287,  286,  285,  284,  283,  282,  279,  278,  276,  275,
      271,  270,  269,  268,  266,  265,  264,  263,  262,  261,
      260,  258,  257,  254,  253,  252,  251,  250,  248,  241,
      240,  239,  227,  225,  224,  223,  222,  221,  220,  219,
      218,  217,  216,  214,  213,  212,  210,  207,  205,  204,
      203,  202,  201,  200,  198,  197,  196,  195,  194,  193,
      192,  191,  190,  189,  183,  182,  181,  180,  179,  178,
      177,  176,  174,  173,  172,  171,  169,  168,  167,  166,

      164,  162,  161,  159,  158,  157,  155,  153,  152,  151,
      150,  149,  148,  147,  146,  145,  144,  142,  138,  136,
      134,  133,  130,  129,  117,  116,  109,  106,  102,  101,
       97,   96,   95,   94,   92,   90,   88,   87,   86,   85,
       84,   77,   68,   62,   61,   59,   58,   56,   55,   54,
       53,   50,   47,   44,   19,   15,    7,    6,    5,    3,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,

      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546,  546,  546,  546,  546,  546,  546,  546,  546,
      546,  546
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
	    if ( yy_current_state >= 547 ) then
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
		    if ( yy_current_state >= 547 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 546 ) then
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


  -- ParaSail reserved words
when 16 => 
--# line 362 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABS_kw);


when 17 => 
--# line 367 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 18 => 
--# line 372 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALL_kw);


when 19 => 
--# line 377 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AND_kw);


when 20 => 
--# line 382 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BEGIN_kw);


when 21 => 
--# line 387 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := BLOCK_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (BLOCK_kw);


when 22 => 
--# line 397 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CASE_kw);


when 23 => 
--# line 402 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 24 => 
--# line 407 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 25 => 
--# line 412 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONST_kw);


when 26 => 
--# line 417 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONTINUE_kw);


when 27 => 
--# line 422 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EACH_kw);


when 28 => 
--# line 427 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := ELSE_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (ELSE_kw);


when 29 => 
--# line 437 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""elseif""");
	return (ELSIF_kw);


when 30 => 
--# line 443 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
        --  Allow "elif" as an alias for "elsif" for Python-like syntax
	return (ELSIF_kw);


when 31 => 
--# line 449 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""else if""");
	return (ELSIF_kw);


when 32 => 
--# line 455 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ELSIF_kw);


when 33 => 
--# line 460 "parasail_lex.l"

        unput('f'); unput('i');
	ECHO_L("end"); ENTER(CHARLIT); 
        yylval := Create_Token("end");
	yyerror("Use ""end if"" rather than ""endif""");
	return (END_kw);


when 34 => 
--# line 467 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 35 => 
--# line 472 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXIT_kw);


when 36 => 
--# line 477 "parasail_lex.l"

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


when 37 => 
--# line 490 "parasail_lex.l"

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
--# line 503 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXTENDS_kw);


when 39 => 
--# line 508 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FOR_kw);


when 40 => 
--# line 513 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FORWARD_kw);


when 41 => 
--# line 518 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 42 => 
--# line 523 "parasail_lex.l"
  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 43 => 
--# line 528 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GLOBAL_kw);


when 44 => 
--# line 533 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IF_kw);


when 45 => 
--# line 538 "parasail_lex.l"

	ECHO_L; ENTER(AFTER_IMPLEMENTS); 
        --  TBD: We might want to return a NEWLINE first.
	yylval := Create_Token;
	return (IMPLEMENTS_kw);


when 46 => 
--# line 544 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 47 => 
--# line 549 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IN_kw);


when 48 => 
--# line 554 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 49 => 
--# line 559 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := IS_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (IS_kw);


when 50 => 
--# line 569 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LAMBDA_kw);


when 51 => 
--# line 574 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LOCKED_kw);


when 52 => 
--# line 579 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := LOOP_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (LOOP_kw);


when 53 => 
--# line 589 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (MOD_kw);


when 54 => 
--# line 594 "parasail_lex.l"

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


when 55 => 
--# line 608 "parasail_lex.l"

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
--# line 622 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NOT_kw);


when 57 => 
--# line 627 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NULL_kw);


when 58 => 
--# line 632 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := OF_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (OF_kw);


when 59 => 
--# line 642 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OP_kw);


when 60 => 
--# line 647 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPT_kw);


when 61 => 
--# line 652 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPTIONAL_kw);


when 62 => 
--# line 657 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OR_kw);


when 63 => 
--# line 662 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRIVATE_kw);


when 64 => 
--# line 667 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (QUEUED_kw);


when 65 => 
--# line 672 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REF_kw);


when 66 => 
--# line 677 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REM_kw);


when 67 => 
--# line 682 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RETURN_kw);


when 68 => 
--# line 687 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REVERSE_kw);


when 69 => 
--# line 692 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SOME_kw);


when 70 => 
--# line 697 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 71 => 
--# line 707 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 72 => 
--# line 717 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TYPE_kw);


when 73 => 
--# line 722 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (UNTIL_kw);


when 74 => 
--# line 727 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VAR_kw);


when 75 => 
--# line 732 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WHILE_kw);


when 76 => 
--# line 737 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WITH_kw);


when 77 => 
--# line 742 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (XOR_kw);


  -- Match all the compound ParaSail delimiters. 
when 78 => 
--# line 749 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMPARE);


when 79 => 
--# line 754 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(EQ);


when 80 => 
--# line 759 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(NEQ);


when 81 => 
--# line 764 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GEQ);


when 82 => 
--# line 769 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LEQ);


when 83 => 
--# line 774 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT);


when 84 => 
--# line 779 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER);


when 85 => 
--# line 784 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(ASSIGN);


when 86 => 
--# line 789 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MOVE);


when 87 => 
--# line 794 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(SWAP);


when 88 => 
--# line 799 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOT_DOT);


when 89 => 
--# line 804 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_INTERVAL);


when 90 => 
--# line 809 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_CLOSED_INTERVAL);


when 91 => 
--# line 814 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(CLOSED_OPEN_INTERVAL);


when 92 => 
--# line 819 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOUBLE_COLON);


when 93 => 
--# line 824 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);


when 94 => 
--# line 829 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GIVES);


when 95 => 
--# line 834 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(IMPLIES);


when 96 => 
--# line 839 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 97 => 
--# line 844 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 98 => 
--# line 849 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_ASSIGN);


when 99 => 
--# line 854 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_ASSIGN);


when 100 => 
--# line 859 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(TIMES_ASSIGN);


when 101 => 
--# line 864 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DIVIDE_ASSIGN);


when 102 => 
--# line 869 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER_ASSIGN);


when 103 => 
--# line 874 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_ASSIGN);


when 104 => 
--# line 879 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_MOVE);


when 105 => 
--# line 884 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_ASSIGN);


when 106 => 
--# line 889 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_ASSIGN);


when 107 => 
--# line 894 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(XOR_ASSIGN);


when 108 => 
--# line 899 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT_ASSIGN);


when 109 => 
--# line 904 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(RSHIFT_ASSIGN);


when 110 => 
--# line 909 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_BASED_OP);


  -- Match all the ParaSail single-character delimiters.
when 111 => 
--# line 916 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(PRIME);


when 112 => 
--# line 921 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
        Paren_Count := Paren_Count + 1;
        --  Push the paren count after bumping it
        Unquote_Top := Unquote_Top + 1;
        Unquote_Stack (Unquote_Top) := Paren_Count;
	yylval := Create_Token;
	return('(');


when 113 => 
--# line 930 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
        Paren_Count := Paren_Count + 1;
	yylval := Create_Token;
	return('(');


when 114 => 
--# line 936 "parasail_lex.l"

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


when 115 => 
--# line 954 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('[');


when 116 => 
--# line 959 "parasail_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(']');


when 117 => 
--# line 964 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('<');


when 118 => 
--# line 969 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('>');


when 119 => 
--# line 974 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);  --  or L_SET in PARython mode


when 120 => 
--# line 979 "parasail_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);


when 121 => 
--# line 984 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);  --  or R_SET in PARython mode


when 122 => 
--# line 989 "parasail_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);


when 123 => 
--# line 994 "parasail_lex.l"

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


when 124 => 
--# line 1014 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('+');


when 125 => 
--# line 1019 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(',');


when 126 => 
--# line 1024 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('-');


when 127 => 
--# line 1029 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('.');


when 128 => 
--# line 1034 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('/');


when 129 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1039 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
        Bracketing_Token := ':';
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(EOL_COLON);


when 130 => 
--# line 1049 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(':');


when 131 => 
--# line 1054 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(';');


when 132 => 
--# line 1059 "parasail_lex.l"

	ECHO_L;
        --  Stay in AFTER_UNQUOTE start state
	yylval := Create_Token;
	return('|');


when 133 => 
--# line 1065 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('|');


when 134 => 
--# line 1070 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('?');


when 135 => 
--# line 1075 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');


when 136 => 
--# line 1081 "parasail_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);


  -- Enumeration literals
when 137 => 
--# line 1088 "parasail_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Enum_Literal);


  -- Decimal numeric literals
when 138 => 
--# line 1095 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 139 => 
--# line 1101 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


  -- Based numeric literals.
when 140 => 
--# line 1109 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 141 => 
--# line 1115 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


when 142 => 
--# line 1121 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


when 143 => 
--# line 1127 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


  -- Match all valid character literals.  See Ada LRM 2.6.
when 144 => 
--# line 1135 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Char_Literal);


  -- Match all valid string literals.  See Ada LRM 2.6.
when 145 => 
--# line 1143 "parasail_lex.l"

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
when 146 => 
--# line 1171 "parasail_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 147 => 
--# line 1173 "parasail_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 148 => 
--# line 1175 "parasail_lex.l"

        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;


when 149 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1182 "parasail_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 150 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1192 "parasail_lex.l"

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
--# line 1202 "parasail_lex.l"

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
--# line 1212 "parasail_lex.l"

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
--# line 1222 "parasail_lex.l"

        --  Ignore outdent for label
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 154 => 
--# line 1232 "parasail_lex.l"

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
--# line 1320 "parasail_lex.l"

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
--# line 1357 "parasail_lex.l"

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
when 157 => 
--# line 1386 "parasail_lex.l"

        --  TBD: Should we return NEWLINE if this line is entirely blank?
        if Echo_Input then
           text_io.new_line;
        end if;
        Display_Linenum;


  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.
when 158 => 
--# line 1397 "parasail_lex.l"
ECHO_L; 
	    yyerror("lexical error: '" &
	      parasail_lex_dfa.yytext & "'");


when 159 => 
--# line 1402 "parasail_lex.l"
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
--# line 1402 "parasail_lex.l"

end parasail_lex;


