
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
YY_END_OF_BUFFER : constant := 162;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
TICK : constant := 1;
CHARLIT : constant := 2;
AFTER_IMPLEMENTS : constant := 3;
AFTER_UNQUOTE : constant := 4;
OUTDENTING : constant := 5;
RESCANNING : constant := 6;
yy_accept : constant array(0..555) of short :=
    (   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  162,  160,  150,  159,  160,  160,
      160,  115,  116,  125,  126,  127,  128,  129,  130,  140,
      140,  132,  133,  119,  137,  120,  136,  138,  117,  118,
      138,  138,  138,  138,  138,  138,  138,  138,  138,  138,
      138,  138,  138,  138,  138,  138,  138,  138,  138,  138,
      138,  121,  135,  123,    6,  138,  138,  138,  138,  138,
      138,  138,  138,  138,  138,  135,  113,  160,    6,  161,
      114,  134,  156,  156,  156,  156,  156,  156,  161,  161,
      161,  161,  161,  161,  161,  161,  161,  161,  161,  150,

      159,    0,    0,    0,   81,    0,  147,    0,  148,  139,
       85,  102,  112,  100,  149,  101,   96,  112,   89,  103,
        0,    0,  140,    0,    0,    0,    0,    0,  131,    0,
      112,    0,   94,   86,    0,   84,   83,    0,  124,   80,
       95,   79,   82,    0,  138,  138,  138,  138,  138,  138,
      138,  138,  138,  138,  138,  138,  138,  138,  138,  138,
      138,   45,  138,   48,   50,  138,  138,  138,  138,  138,
      138,   59,   60,   63,  138,  138,  138,  138,  138,  138,
      138,  138,  138,  138,  138,  122,  105,   99,    6,    0,
        0,    0,    0,    0,    0,  138,  138,  138,  138,  138,

      138,  138,  138,   48,  138,    6,  138,  138,   99,    0,
        0,    6,    0,  155,    0,    0,    0,  154,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,   15,    0,
        0,   98,  148,  139,  104,  149,   93,    0,  141,  145,
        0,  140,  144,    0,   92,  110,   87,   88,  106,   97,
      111,   17,   19,   20,  138,  138,  138,  138,  138,   43,
      138,  138,  138,   35,  138,  138,  138,   40,  138,  138,
      138,  138,  138,  138,  138,   54,   56,   57,  138,   61,
      108,  138,  138,   66,   67,  138,  138,  138,  138,  138,
      138,   75,  138,  138,   78,    4,    0,    0,    0,    0,

        0,   17,  138,  138,    6,    6,  138,  138,  138,  138,
       56,    6,  138,    3,  146,    0,    0,    0,    0,    0,
        0,    0,    0,   12,   10,    0,    0,    0,    0,   55,
       16,    0,  142,    0,    0,    0,  141,    0,    0,  145,
        0,  140,    0,  144,    0,    0,   91,   90,  138,  107,
      138,  138,   23,  138,  138,  138,  138,   28,   31,   29,
      138,  138,   36,  138,  138,  138,   42,  138,  138,  138,
      138,  138,  138,   53,   58,  138,  138,  138,  138,  138,
       70,   72,   73,  138,  138,   77,  109,    0,    1,    0,
      138,  138,  138,  138,    6,  138,  138,   72,    0,    0,

        0,  153,    0,    0,    0,    0,    0,   11,    0,    0,
       71,    0,    0,    0,  141,  138,   21,   22,   24,  138,
       26,  138,    0,  138,   33,   34,  138,  138,  138,  138,
      138,  138,  138,  138,  138,  138,  138,  138,  138,  138,
       74,   76,    0,  138,    6,  138,  138,  138,  138,    5,
        0,  151,    0,    8,    0,    0,    0,    0,    0,  142,
      143,    0,    0,  141,    0,  138,  138,  138,    0,   30,
      138,  138,  138,   44,  138,   47,  138,   51,   52,  138,
      138,   65,   68,  138,    0,  138,  138,  138,    6,  138,
        0,    0,    0,    0,   13,    0,  142,    0,    0,  138,

      138,  138,   32,   38,   39,   41,  138,  138,  138,   64,
       69,    0,  138,  138,   38,  138,    0,    0,    0,   37,
        0,    0,  143,   18,  138,   27,  138,  138,   62,    0,
        6,  138,    2,  138,    0,    7,    0,    0,  143,    0,
      138,  138,   49,  138,    6,    0,    0,   14,   25,   46,
        6,    0,    9,  152,    0
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

yy_base : constant array(0..574) of short :=
    (   0,
        0,   60,  856,  855,  854,  853,  852,   61,   57,   58,
       69,   72,    0,   98,  859,  865,   74,  865,  836,   90,
       66,  865,  865,   60,   70,  865,   87,   72,  112,  152,
      135,  180,  865,  133,   82,  104,  865,    0,  865,  865,
      128,  118,   95,  818,  160,  112,  811,  125,   96,  807,
      150,  157,  803,  799,  813,  803,   44,  803,  814,  158,
      800,  824,  101,  865,  216,  167,  176,  807,  181,  132,
      189,  183,  193,  806,  192,  188,  865,  813,  211,  865,
      865,  865,  865,    0,  797,  803,  799,  780,  803,   46,
      799,  192,  783,  199,  797,  786,  795,  791,  772,  250,

      865,  254,  817,  814,  865,  237,  865,  826,    0,    0,
      806,  865,  865,  865,    0,  865,  865,  812,  805,  865,
        0,  255,  260,  262,  271,    0,  264,  288,  865,  258,
      806,  808,  865,  865,  808,  800,  274,  799,  865,  797,
      865,  865,  865,  797,    0,  766,  772,  778,  774,  766,
      761,  777,  764,  770,  772,  104,  770,  257,  756,  759,
      757,    0,  755,  750,    0,  756,  257,  763,  744,  746,
      753,    0,  744,  774,  752,  755,  261,  747,  753,  742,
      737,  738,  745,  734,  735,  865,  865,  865,    0,    0,
      728,  732,  744,  740,  721,  728,  744,  731,  737,  738,

      259,  728,  725,  720,  716,  718,  269,  731,  315,  761,
      313,    0,  720,  865,  718,  710,  726,  865,  712,  728,
      715,  721,  722,  710,  711,  708,  703,  699,  865,  714,
      714,  865,    0,    0,  865,    0,  865,  296,  308,  310,
      313,  315,  719,  748,  263,  865,  865,  865,  865,  865,
      865,  697,    0,  727,  705,  710,  707,  693,  286,    0,
      702,  703,  231,  699,  688,  692,  700,  682,  700,  700,
      289,  696,  698,  689,  683,    0,    0,    0,  686,  687,
      865,  674,  674,    0,    0,  673,  687,  686,  677,  684,
      679,    0,  676,  678,  697,  865,  669,  342,  661,  677,

      347,  662,  662,  303,    0,  670,  664,  674,  309,  671,
      356,    0,  662,  865,  865,  701,  656,  661,  359,  658,
      651,  651,  665,  865,  865,  653,  663,  651,  659,  865,
      865,  650,  325,    0,  358,  359,  350,  358,  361,  363,
      367,  369,  376,  663,  662,  691,  865,  865,  642,  865,
      645,  647,    0,  638,  635,  635,  644,    0,    0,  365,
      646,  645,    0,  633,  636,  647,    0,  646,  641,  628,
      627,  639,  637,    0,    0,  627,  639,  634,  621,  620,
        0,    0,    0,  625,  630,    0,  865,  620,  865,  620,
      615,  613,  610,  612,    0,  611,  610,  395,  653,  401,

      620,  865,  407,  607,  605,  602,  604,  865,  603,  602,
      865,  400,  383,  383,  389,  617,    0,    0,    0,  600,
        0,  603,  606,  608,    0,    0,  594,  608,  594,  599,
      597,  589,  601,  605,  601,  591,  584,  598,  588,  582,
        0,    0,  582,  597,    0,  580,  577,  576,  588,  865,
      581,  865,  591,  865,  574,  571,  570,  582,  402,  406,
      386,  388,  408,  409,  411,  584,  568,  562,  575,    0,
      562,  557,  568,    0,  566,    0,  569,    0,    0,  555,
      547,    0,    0,  543,  528,  537,  505,  489,    0,  498,
      482,  473,  458,  456,  865,  472,  413,  415,  426,  452,

      430,  429,  865,    0,    0,    0,  420,  429,  419,    0,
        0,  411,  409,  422,  432,  408,  390,  335,  343,  865,
      334,  430,  432,    0,  306,    0,  289,  297,    0,  449,
        0,  266,  865,  259,  194,  865,  178,  151,  437,  439,
      123,   87,    0,   61,    0,   27,   21,  865,    0,    0,
        0,  456,  865,  865,  865,  470,  481,  492,  502,  505,
      516,  527,  538,  542,  553,  557,  559,  568,  576,  580,
      591,  599,  603,  609
    ) ;

yy_def : constant array(0..574) of short :=
    (   0,
      555,    1,    1,    2,    1,    2,    1,    2,  556,  556,
      557,  557,  556,  556,  555,  555,  555,  555,  555,  558,
      559,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  560,  555,  555,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  555,  555,  555,  555,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  555,  555,  561,   65,  555,
      555,  555,  555,  562,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,

      555,  555,  555,  555,  555,  558,  555,  558,  563,  564,
      555,  555,  555,  555,  565,  555,  555,  555,  555,  555,
      566,  555,  555,  555,  555,  567,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  555,  555,  555,   65,  568,
      555,  555,  555,  555,  555,  560,  560,  560,  560,  560,

      560,  560,  560,  560,  560,  560,  560,  560,  555,  555,
      561,   79,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  563,  564,  555,  565,  555,  569,  555,  555,
      555,  555,  570,  571,  555,  555,  555,  555,  555,  555,
      555,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      555,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  555,  555,  555,  555,  555,

      555,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  555,  555,  572,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  573,  569,  569,  555,  555,  555,  555,
      555,  555,  555,  570,  570,  571,  555,  555,  560,  555,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  555,  555,  555,  555,
      560,  560,  560,  560,  560,  560,  560,  560,  572,  555,

      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  574,  555,  555,  560,  560,  560,  560,  560,
      560,  560,  555,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  555,  560,  560,  560,  560,  560,  560,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  574,  574,  555,  555,  560,  560,  560,  555,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  555,  560,  560,  560,  560,  560,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  560,

      560,  560,  555,  560,  560,  560,  560,  560,  560,  560,
      560,  555,  560,  560,  560,  560,  555,  555,  555,  555,
      555,  555,  555,  560,  560,  560,  560,  560,  560,  555,
      560,  560,  555,  560,  555,  555,  555,  555,  555,  555,
      560,  560,  560,  560,  560,  555,  555,  555,  560,  560,
      560,  555,  555,  555,    0,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555
    ) ;

yy_nxt : constant array(0..926) of short :=
    (   0,
       16,   17,   18,   17,   19,   20,   21,   16,   22,   23,
       24,   25,   26,   27,   28,   29,   30,   31,   32,   33,
       34,   35,   36,   37,   38,   38,   38,   38,   38,   39,
       16,   40,   16,   16,   41,   42,   43,   44,   45,   46,
       47,   38,   48,   38,   49,   50,   51,   52,   53,   54,
       55,   56,   57,   58,   59,   60,   61,   38,   62,   63,
       64,   65,   79,   65,   79,   81,   81,  109,   78,  109,
      111,   80,  109,  553,   80,  100,  101,  100,  552,   84,
      102,  112,   84,  118,  113,  179,  119,  103,  113,  104,
      220,  114,  107,  221,   66,  107,   67,   68,   69,   70,

      115,  180,   71,  140,  141,  142,   72,   73,  116,  117,
       74,   85,   75,  551,   85,   86,   82,   82,   86,   76,
      108,   87,  187,  107,   87,  143,  144,  109,   88,  151,
      166,   88,   89,  120,   90,   91,   92,   93,  550,  152,
       94,  121,  153,  167,   95,   96,  262,  135,   97,  122,
       98,  123,  123,  136,  137,  263,  149,   99,  121,  159,
      188,  125,  150,  146,  162,  160,  122,  127,  123,  123,
      163,  164,  147,  125,  148,  549,  165,  124,  125,  159,
      126,  128,  129,  128,  127,  202,  130,  124,  169,  548,
      125,  131,  138,  139,  155,  132,  172,  170,  133,  183,

      184,  134,  196,  171,  156,  173,  157,  174,  126,  187,
      151,  147,  212,  148,  212,  155,  158,  189,  101,  189,
      197,  205,  102,  198,  547,  156,  190,  200,  162,  103,
      170,  104,  172,  208,  203,  204,  171,  201,  223,  107,
      165,  206,  107,  174,  226,  227,  546,  209,  224,  180,
      213,  100,  101,  100,  191,  109,  102,  109,  192,  244,
      109,  244,  193,  103,  244,  104,  121,  108,  194,  360,
      107,  239,  239,  361,  122,  195,  123,  123,  240,  240,
      123,  123,  241,  347,  241,  348,  125,  242,  242,  128,
      129,  128,  127,  274,  130,  247,  248,  545,  125,  265,

      284,  265,  333,  132,  275,  266,  285,  307,  312,  267,
      334,  267,  544,  286,  285,  287,  314,  314,  314,  316,
      210,  286,  355,  287,  337,  337,  340,  340,  336,  242,
      242,  342,  342,  369,  338,  543,  370,  356,  357,  393,
      339,  542,  341,  389,  389,  389,  338,  343,  314,  314,
      314,  412,  541,  369,  356,  357,  396,  314,  314,  314,
      402,  402,  402,  412,  333,  555,  337,  337,  423,  414,
      538,  414,  334,  555,  415,  415,  338,  337,  337,  340,
      340,  537,  339,  340,  340,  342,  342,  536,  338,  461,
      336,  555,  342,  342,  461,  341,  314,  314,  314,  415,

      415,  343,  450,  450,  450,  464,  464,  424,  452,  452,
      452,  459,  499,  459,  555,  463,  460,  460,  460,  460,
      463,  465,  497,  497,  499,  464,  464,  464,  464,  497,
      497,  497,  497,  533,  533,  533,  535,  522,  498,  522,
      555,  465,  523,  523,  534,  498,  523,  523,  539,  539,
      533,  533,  533,  539,  539,  539,  539,  554,  554,  554,
      532,  531,  530,  529,  540,  528,  527,  526,  525,  540,
       80,   80,   80,   80,   80,   80,   80,   80,   80,   80,
       80,   83,   83,   83,   83,   83,   83,   83,   83,   83,
       83,   83,  106,  106,  106,  106,  106,  106,  106,  106,

      106,  106,  106,  110,  524,  110,  521,  520,  519,  518,
      110,  110,  145,  145,  145,  145,  210,  210,  210,  210,
      517,  210,  210,  210,  210,  210,  210,  214,  214,  214,
      214,  214,  516,  214,  214,  214,  214,  214,  233,  233,
      515,  233,  233,  233,  233,  233,  233,  233,  233,  234,
      234,  234,  234,  236,  236,  514,  236,  236,  236,  236,
      236,  236,  236,  236,  238,  238,  243,  243,  296,  296,
      296,  296,  296,  513,  296,  296,  296,  296,  296,  335,
      512,  511,  335,  335,  335,  510,  335,  344,  344,  509,
      344,  346,  346,  346,  346,  346,  346,  346,  346,  346,

      346,  346,  399,  508,  507,  506,  399,  399,  505,  399,
      413,  413,  462,  504,  503,  502,  462,  462,  501,  462,
      500,  496,  495,  494,  493,  492,  491,  490,  489,  488,
      487,  486,  485,  484,  483,  482,  481,  480,  479,  478,
      477,  476,  475,  474,  473,  472,  471,  470,  469,  468,
      467,  466,  458,  457,  456,  455,  454,  453,  451,  210,
      449,  448,  447,  446,  445,  444,  301,  443,  442,  441,
      440,  439,  438,  437,  436,  435,  434,  433,  432,  431,
      430,  429,  428,  427,  426,  425,  422,  421,  420,  419,
      418,  417,  416,  129,  555,  345,  411,  410,  409,  408,

      407,  406,  405,  404,  403,  401,  400,  210,  398,  397,
      395,  394,  362,  392,  391,  390,  301,  388,  387,  386,
      385,  384,  383,  382,  381,  380,  379,  378,  377,  376,
      375,  374,  373,  372,  371,  368,  367,  366,  365,  364,
      363,  362,  359,  358,  354,  353,  352,  351,  350,  349,
      129,  345,  332,  331,  330,  329,  328,  327,  326,  325,
      324,  323,  322,  321,  320,  319,  318,  317,  315,  313,
      280,  311,  310,  309,  308,  306,  305,  304,  303,  302,
      301,  300,  299,  298,  297,  295,  294,  293,  292,  291,
      290,  289,  288,  283,  282,  281,  280,  279,  278,  277,

      276,  273,  272,  271,  270,  269,  268,  264,  261,  260,
      259,  258,  257,  256,  255,  254,  253,  252,  251,  250,
      249,  246,  245,  244,  113,  237,  113,  235,  555,  109,
      115,  232,  231,  230,  229,  228,  225,  222,  219,  218,
      217,  216,  215,  211,  207,  199,  186,  185,  182,  181,
      178,  177,  176,  175,  168,  161,  154,  105,  555,   78,
       78,   78,   77,   77,   15,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,

      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555
    ) ;

yy_chk : constant array(0..926) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    8,    2,    8,    9,   10,   21,    8,   21,
       24,   11,   21,  547,   12,   17,   17,   17,  546,   11,
       17,   24,   12,   28,   25,   57,   28,   17,   25,   17,
       90,   25,   20,   90,    2,   20,    2,    2,    2,    2,

       27,   57,    2,   35,   35,   35,    2,    2,   27,   27,
        2,   11,    2,  544,   12,   11,    9,   10,   12,    2,
       20,   11,   63,   20,   12,   36,   36,   29,   11,   43,
       49,   12,   14,   29,   14,   14,   14,   14,  542,   43,
       14,   31,   43,   49,   14,   14,  156,   34,   14,   31,
       14,   31,   31,   34,   34,  156,   42,   14,   30,   46,
       63,   31,   42,   41,   48,   46,   30,   31,   30,   30,
       48,   48,   41,   31,   41,  541,   48,   30,   30,   70,
       30,   32,   32,   32,   30,   70,   32,   30,   51,  538,
       30,   32,   34,   34,   45,   32,   52,   51,   32,   60,

       60,   32,   66,   51,   45,   52,   45,   52,   30,   76,
       67,   66,   79,   66,   79,   69,   45,   65,   65,   65,
       67,   72,   65,   67,  537,   69,   65,   69,   71,   65,
       72,   65,   73,   75,   71,   71,   72,   69,   92,  106,
       71,   73,  106,   73,   94,   94,  535,   76,   92,   75,
       79,  100,  100,  100,   65,  102,  100,  102,   65,  130,
      102,  130,   65,  100,  130,  100,  123,  106,   65,  263,
      106,  122,  122,  263,  123,   65,  123,  123,  124,  124,
      127,  127,  125,  245,  125,  245,  123,  125,  125,  128,
      128,  128,  123,  167,  128,  137,  137,  534,  123,  158,

      177,  201,  238,  128,  167,  158,  177,  201,  207,  158,
      238,  201,  532,  177,  207,  177,  209,  209,  209,  211,
      211,  207,  259,  207,  239,  239,  240,  240,  238,  241,
      241,  242,  242,  271,  239,  528,  271,  259,  259,  304,
      239,  527,  240,  298,  298,  298,  239,  242,  301,  301,
      301,  333,  525,  309,  304,  304,  309,  311,  311,  311,
      319,  319,  319,  333,  335,  336,  337,  337,  360,  338,
      521,  338,  335,  336,  338,  338,  337,  339,  339,  340,
      340,  519,  337,  341,  341,  342,  342,  518,  337,  413,
      335,  336,  343,  343,  462,  340,  398,  398,  398,  414,

      414,  342,  400,  400,  400,  415,  415,  360,  403,  403,
      403,  412,  461,  412,  463,  413,  412,  412,  459,  459,
      462,  415,  460,  460,  461,  464,  464,  465,  465,  497,
      497,  498,  498,  515,  515,  515,  517,  499,  460,  499,
      463,  464,  499,  499,  516,  497,  522,  522,  523,  523,
      530,  530,  530,  539,  539,  540,  540,  552,  552,  552,
      514,  513,  512,  509,  523,  508,  507,  502,  501,  539,
      556,  556,  556,  556,  556,  556,  556,  556,  556,  556,
      556,  557,  557,  557,  557,  557,  557,  557,  557,  557,
      557,  557,  558,  558,  558,  558,  558,  558,  558,  558,

      558,  558,  558,  559,  500,  559,  496,  494,  493,  492,
      559,  559,  560,  560,  560,  560,  561,  561,  561,  561,
      491,  561,  561,  561,  561,  561,  561,  562,  562,  562,
      562,  562,  490,  562,  562,  562,  562,  562,  563,  563,
      488,  563,  563,  563,  563,  563,  563,  563,  563,  564,
      564,  564,  564,  565,  565,  487,  565,  565,  565,  565,
      565,  565,  565,  565,  566,  566,  567,  567,  568,  568,
      568,  568,  568,  486,  568,  568,  568,  568,  568,  569,
      485,  484,  569,  569,  569,  481,  569,  570,  570,  480,
      570,  571,  571,  571,  571,  571,  571,  571,  571,  571,

      571,  571,  572,  477,  475,  473,  572,  572,  472,  572,
      573,  573,  574,  471,  469,  468,  574,  574,  467,  574,
      466,  458,  457,  456,  455,  453,  451,  449,  448,  447,
      446,  444,  443,  440,  439,  438,  437,  436,  435,  434,
      433,  432,  431,  430,  429,  428,  427,  424,  423,  422,
      420,  416,  410,  409,  407,  406,  405,  404,  401,  399,
      397,  396,  394,  393,  392,  391,  390,  388,  385,  384,
      380,  379,  378,  377,  376,  373,  372,  371,  370,  369,
      368,  366,  365,  364,  362,  361,  357,  356,  355,  354,
      352,  351,  349,  346,  345,  344,  332,  329,  328,  327,

      326,  323,  322,  321,  320,  318,  317,  316,  313,  310,
      308,  307,  306,  303,  302,  300,  299,  297,  295,  294,
      293,  291,  290,  289,  288,  287,  286,  283,  282,  280,
      279,  275,  274,  273,  272,  270,  269,  268,  267,  266,
      265,  264,  262,  261,  258,  257,  256,  255,  254,  252,
      244,  243,  231,  230,  228,  227,  226,  225,  224,  223,
      222,  221,  220,  219,  217,  216,  215,  213,  210,  208,
      206,  205,  204,  203,  202,  200,  199,  198,  197,  196,
      195,  194,  193,  192,  191,  185,  184,  183,  182,  181,
      180,  179,  178,  176,  175,  174,  173,  171,  170,  169,

      168,  166,  164,  163,  161,  160,  159,  157,  155,  154,
      153,  152,  151,  150,  149,  148,  147,  146,  144,  140,
      138,  136,  135,  132,  131,  119,  118,  111,  108,  104,
      103,   99,   98,   97,   96,   95,   93,   91,   89,   88,
       87,   86,   85,   78,   74,   68,   62,   61,   59,   58,
       56,   55,   54,   53,   50,   47,   44,   19,   15,    7,
        6,    5,    4,    3,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,

      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555,  555,  555,  555,  555,
      555,  555,  555,  555,  555,  555
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
	    if ( yy_current_state >= 556 ) then
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
		    if ( yy_current_state >= 556 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 555 ) then
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
	return(ANGLE_DOT_DOT_ANGLE);


when 91 => 
--# line 815 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_INTERVAL);


when 92 => 
--# line 820 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_CLOSED_INTERVAL);


when 93 => 
--# line 825 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(CLOSED_OPEN_INTERVAL);


when 94 => 
--# line 830 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOUBLE_COLON);


when 95 => 
--# line 835 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);


when 96 => 
--# line 840 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GIVES);


when 97 => 
--# line 845 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(IMPLIES);


when 98 => 
--# line 850 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 99 => 
--# line 855 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 100 => 
--# line 860 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_ASSIGN);


when 101 => 
--# line 865 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_ASSIGN);


when 102 => 
--# line 870 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(TIMES_ASSIGN);


when 103 => 
--# line 875 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DIVIDE_ASSIGN);


when 104 => 
--# line 880 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER_ASSIGN);


when 105 => 
--# line 885 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_ASSIGN);


when 106 => 
--# line 890 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_MOVE);


when 107 => 
--# line 895 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_ASSIGN);


when 108 => 
--# line 900 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_ASSIGN);


when 109 => 
--# line 905 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(XOR_ASSIGN);


when 110 => 
--# line 910 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT_ASSIGN);


when 111 => 
--# line 915 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(RSHIFT_ASSIGN);


when 112 => 
--# line 920 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_BASED_OP);


  -- Match all the ParaSail single-character delimiters.
when 113 => 
--# line 927 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(PRIME);


when 114 => 
--# line 932 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
        Paren_Count := Paren_Count + 1;
        --  Push the paren count after bumping it
        Unquote_Top := Unquote_Top + 1;
        Unquote_Stack (Unquote_Top) := Paren_Count;
	yylval := Create_Token;
	return('(');


when 115 => 
--# line 941 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
        Paren_Count := Paren_Count + 1;
	yylval := Create_Token;
	return('(');


when 116 => 
--# line 947 "parasail_lex.l"

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


when 117 => 
--# line 965 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('[');


when 118 => 
--# line 970 "parasail_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(']');


when 119 => 
--# line 975 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('<');


when 120 => 
--# line 980 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('>');


when 121 => 
--# line 985 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);  --  or L_SET in PARython mode


when 122 => 
--# line 990 "parasail_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);


when 123 => 
--# line 995 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);  --  or R_SET in PARython mode


when 124 => 
--# line 1000 "parasail_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);


when 125 => 
--# line 1005 "parasail_lex.l"

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


when 126 => 
--# line 1025 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('+');


when 127 => 
--# line 1030 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(',');


when 128 => 
--# line 1035 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('-');


when 129 => 
--# line 1040 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('.');


when 130 => 
--# line 1045 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('/');


when 131 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1050 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
        Bracketing_Token := ':';
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(EOL_COLON);


when 132 => 
--# line 1060 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(':');


when 133 => 
--# line 1065 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(';');


when 134 => 
--# line 1070 "parasail_lex.l"

	ECHO_L;
        --  Stay in AFTER_UNQUOTE start state
	yylval := Create_Token;
	return('|');


when 135 => 
--# line 1076 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('|');


when 136 => 
--# line 1081 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('?');


when 137 => 
--# line 1086 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');


when 138 => 
--# line 1092 "parasail_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);


  -- Enumeration literals
when 139 => 
--# line 1099 "parasail_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Enum_Literal);


  -- Decimal numeric literals
when 140 => 
--# line 1106 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 141 => 
--# line 1112 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


  -- Based numeric literals.
when 142 => 
--# line 1120 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 143 => 
--# line 1126 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


when 144 => 
--# line 1132 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


when 145 => 
--# line 1138 "parasail_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


  -- Match all valid character literals.  See Ada LRM 2.6.
when 146 => 
--# line 1146 "parasail_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Char_Literal);


  -- Match all valid string literals.  See Ada LRM 2.6.
when 147 => 
--# line 1154 "parasail_lex.l"

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
when 148 => 
--# line 1182 "parasail_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 149 => 
--# line 1184 "parasail_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 150 => 
--# line 1186 "parasail_lex.l"

        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;


when 151 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1193 "parasail_lex.l"

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
--# line 1203 "parasail_lex.l"

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
--# line 1213 "parasail_lex.l"

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
--# line 1223 "parasail_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 155 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1233 "parasail_lex.l"

        --  Ignore outdent for label
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 156 => 
--# line 1243 "parasail_lex.l"

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
--# line 1331 "parasail_lex.l"

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
--# line 1368 "parasail_lex.l"

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
when 159 => 
--# line 1397 "parasail_lex.l"

        --  TBD: Should we return NEWLINE if this line is entirely blank?
        if Echo_Input then
           text_io.new_line;
        end if;
        Display_Linenum;


  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.
when 160 => 
--# line 1408 "parasail_lex.l"
ECHO_L; 
	    yyerror("lexical error: '" &
	      parasail_lex_dfa.yytext & "'");


when 161 => 
--# line 1413 "parasail_lex.l"
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
--# line 1413 "parasail_lex.l"

end parasail_lex;


