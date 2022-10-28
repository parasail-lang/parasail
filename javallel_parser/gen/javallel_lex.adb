
package body javallel_lex is

  --  Indicates whether outdenting should expect a perfect col-count match
  Col_Match_Expected : Boolean := True;

  Max_Depth : constant := 100;
  Indent_Stack : array(Positive range 1 .. Max_Depth) of Natural :=
    (others => 0);
  Bracketing_Token_Stack : array(Positive range 1 .. Max_Depth) of Token :=
    (others => Error);

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
YY_END_OF_BUFFER : constant := 169;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
TICK : constant := 1;
CHARLIT : constant := 2;
AFTER_IMPLEMENTS : constant := 3;
OUTDENTING : constant := 4;
RESCANNING : constant := 5;
yy_accept : constant array(0..593) of short :=
    (   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  169,  167,  157,  166,  167,  167,  167,  143,
      123,  124,  133,  134,  135,  136,  137,  138,  148,  148,
      140,  141,  127,  145,  128,  144,  146,  125,  126,  146,
      146,  146,  146,  146,  146,  146,  146,  146,  146,  146,
      146,  146,  146,  146,  146,  146,  146,  146,  146,  146,
      129,  142,  131,    6,  146,  146,  146,  146,  146,  146,
      146,  146,  146,  146,  146,  142,    6,  122,  167,    6,
      163,  168,  163,  163,  163,  163,  163,  168,  168,  168,
      168,  168,  168,  168,  168,  168,  168,  168,  168,   17,

      157,  166,    0,   88,    0,  155,    0,  147,  119,   92,
      108,  120,  106,  121,  107,  102,   96,  156,  109,    0,
        0,  148,    0,    0,    0,    0,    0,  139,    0,  100,
       93,    0,   91,   90,    0,  132,   87,  101,   86,   89,
        0,  146,  146,  146,  146,   22,  146,  146,  146,  146,
      146,  146,  146,  146,  146,  146,  146,  146,  146,  146,
      146,   48,  146,   51,   53,  146,  146,  146,  146,  146,
      146,   61,   62,   65,  146,  146,  146,  146,  146,  146,
      146,  146,  146,  146,  146,  146,  146,  146,  146,  146,
      146,  130,  111,  105,    6,    0,    0,    0,    0,    0,

        0,  146,  146,  146,  146,  146,   51,  146,    6,  146,
      146,  146,  146,  146,  105,    0,    0,    6,    0,  162,
        0,    0,    0,  161,    0,    0,    0,    0,    0,    0,
        0,   13,    0,    0,    0,    0,    0,  104,  147,  110,
       99,  156,    0,  149,  153,    0,  148,  152,    0,   98,
      116,   94,   95,  112,  103,  117,   18,   20,   21,  146,
      146,  146,  146,  146,  146,  146,  146,  146,  146,  146,
       38,  146,  146,  146,   43,  146,  146,  146,  146,  146,
      146,   56,   58,   59,  146,   63,  114,  146,  146,  146,
      146,   69,   70,  146,  146,  146,  146,  146,  146,  146,

      146,  146,   81,  146,  146,  146,   85,    4,    0,    0,
        0,    0,    0,   18,  146,    6,  146,  146,  146,   58,
      146,  146,  146,  146,  146,    3,  154,    0,    0,    0,
        0,    0,    0,    9,    0,    0,    0,   57,    0,    0,
        0,    0,    0,  150,    0,    0,    0,  149,    0,    0,
      153,    0,  148,    0,  152,    0,    0,   97,  146,  113,
      146,  146,  146,  146,   27,  146,  146,  146,  146,   32,
       35,   33,  146,  146,   39,  146,  146,  146,   45,  146,
      146,  146,  146,  146,  146,   60,  146,  146,  146,  146,
      146,  146,  146,   73,  146,  146,  146,   78,   79,  146,

       82,  146,   84,  115,    1,    0,    0,  146,  146,    6,
      146,  146,  146,  146,  146,   78,    6,    0,    0,  160,
        0,    0,    0,   10,    0,    0,    0,    0,    0,   77,
       16,    0,    0,    0,  149,  146,  146,   24,   25,   26,
       28,   29,  146,  146,  146,   36,   37,  146,   46,  146,
      146,  146,  146,  146,  146,  146,  146,  146,  146,  146,
      146,  146,  146,  146,  146,  146,   80,   83,    0,  146,
        6,  146,  146,  146,  146,  146,    5,    0,  158,    0,
        8,    0,    0,    0,    0,    0,    0,  150,  151,    0,
        0,  149,    0,  146,   23,  146,  146,   34,  146,  146,

       47,  146,   50,  146,   54,   55,  146,  146,  146,   41,
       68,   71,  146,   74,   75,  146,    0,  146,    6,  146,
       41,    6,  146,    0,    0,   11,    0,   40,   14,    0,
      150,    0,    0,  146,  146,   31,   42,   44,  146,  146,
      146,  146,   67,   72,  146,    0,  146,  146,    2,  146,
        0,    0,    0,    0,    0,  151,   19,   30,  146,  146,
       64,   66,  146,    6,  146,  146,    0,    7,    0,    0,
      151,    0,  146,   52,  146,    6,  146,    0,   12,    0,
       49,  146,  146,    0,    0,  146,  146,  159,    0,   76,
        6,   15,    0

    ) ;

yy_ec : constant array(ASCII.NUL..Character'Last) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    2,    2,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    4,    5,    6,    1,    1,    7,    8,    9,
       10,   11,   12,   13,   14,   15,   16,   17,   18,   18,
       18,   18,   18,   18,   18,   18,   18,   19,   20,   21,
       22,   23,   24,    1,   25,   26,   25,   25,   27,   25,
       28,   28,   28,   28,   28,   28,   28,   28,   28,   28,
       28,   28,   28,   28,   28,   28,   28,   29,   28,   28,
       30,   31,   32,    1,   33,    1,   34,   35,   36,   37,

       38,   39,   40,   41,   42,   28,   43,   44,   45,   46,
       47,   48,   49,   50,   51,   52,   53,   54,   55,   56,
       57,   58,   59,   60,   61,    1,    1,    1,    1,    1,
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
        1,    1,    2,    1,    1,    3,    1,    4,    1,    1,
        5,    1,    1,    1,    6,    1,    7,    7,    1,    1,
        1,    5,    1,    1,    8,    8,    8,    9,    9,    1,
        1,    1,   10,    8,    8,    8,    8,    8,    8,    9,
        9,    9,    9,    9,    9,    9,    9,    9,    9,    9,
        9,    9,    9,    9,    9,    9,    9,    9,    1,    1,
        1
    ) ;

yy_base : constant array(0..610) of short :=
    (   0,
        0,   60,  850,  849,  848,  847,  846,   61,   63,   76,
        0,  103,  853,  859,   62,  859,  830,   70,    0,  844,
      859,  859,   59,   55,  859,   62,  835,   64,  159,  129,
      150,  859,  110,   66,   49,  859,    0,  859,  859,  138,
       53,   48,  811,  134,   77,  804,  154,   66,  800,  149,
       90,  145,  793,  807,  154,   69,  798,   70,   92,  796,
      819,  121,  859,  201,  172,  114,  176,  166,  182,  187,
      187,  195,  184,  118,  204,  192,  859,  859,  810,  260,
      859,  859,    0,  795,  801,  797,  777,  801,  791,  788,
      780,  209,  794,  783,  777,  207,  788,  781,  767,  859,

      254,  859,  810,  859,  241,  859,  822,    0,  859,  802,
      859,  859,  859,  859,  859,  859,  802,    0,  859,    0,
      248,  262,  256,  269,    0,  267,  288,  859,  806,  859,
      859,  806,  798,  270,  797,  859,  795,  859,  859,  859,
      795,    0,  765,  771,  777,  762,  772,  764,  772,  758,
      774,  761,  767,  769,  216,  767,   41,  757,  752,  755,
      753,    0,  751,  746,    0,  752,  760,  758,  739,  741,
      748,    0,  739,  768,  739,  746,  752,  748,  249,  740,
      750,  741,  736,  743,  732,  727,  728,  735,  734,  723,
      724,  859,  859,  859,  294,    0,  722,  734,  718,  729,

      709,  717,  733,  729,  719,  716,  711,  707,  709,  725,
      725,  712,  719,  714,  311,  747,  308,    0,  707,  859,
      705,  697,  713,  859,  699,  715,  711,  701,  698,  693,
      689,  859,  708,  708,  695,  702,  697,  859,    0,  859,
      859,    0,  292,  300,  304,  306,  311,  705,  734,  715,
      859,  859,  859,  859,  859,  859,  683,    0,  712,  695,
      690,  695,  696,  691,  677,  279,  693,  685,  686,  240,
      682,  671,  684,  687,  665,  683,  683,  288,  679,  681,
      672,    0,    0,    0,  670,  671,  859,  678,  657,  666,
      656,    0,    0,  655,  669,  668,  653,  652,  667,  656,

      663,  658,    0,  662,  654,  656,  674,  859,  345,  640,
      659,  655,  347,  640,  640,  648,  653,  308,  650,  354,
      643,  634,  649,  638,  646,  859,  859,  632,  637,  356,
      634,  627,  627,  859,  641,  629,  637,  859,  630,  621,
      636,  625,  633,   89,    0,  328,  345,  346,  353,  351,
      357,  359,  363,  365,  636,  635,  664,  859,  616,  859,
      615,  618,  620,  619,    0,  610,  608,  617,  605,    0,
        0,  615,  617,  616,    0,  608,  609,  618,    0,  617,
      612,  599,  598,  610,  608,    0,  598,  600,  609,  600,
      603,  590,  589,    0,  596,  601,  595,    0,    0,  591,

        0,  596,    0,  859,  859,  589,  586,  581,  579,    0,
      579,  578,  585,  584,  584,  383,    0,  385,  586,  859,
      389,  573,  571,  859,  571,  570,  577,  576,  576,  859,
      859,  381,  236,  383,  385,  582,  563,    0,    0,    0,
        0,    0,  568,  569,  573,    0,    0,  574,    0,  560,
      565,  563,  555,  567,  571,  567,  557,  558,  549,  564,
      562,  552,  546,  560,  554,  544,    0,    0,  551,  558,
        0,  539,  551,  553,  552,  537,  859,  541,  859,  551,
      859,  532,  544,  546,  545,  530,  387,  389,  288,  333,
      339,  391,  393,  543,    0,  525,  525,    0,  525,  538,

        0,  536,    0,  539,    0,    0,  538,  533,  532,    0,
        0,    0,  531,    0,    0,  521,  531,  530,    0,  530,
      410,    0,  514,  522,  523,  859,  510,  859,  859,  493,
      397,  399,  409,  484,  497,    0,    0,    0,  482,  475,
      456,  445,    0,    0,  402,  417,  394,  409,  859,  398,
      397,  390,  405,  394,  411,  414,    0,    0,  387,  387,
        0,    0,  355,    0,  356,  347,  310,  859,  315,  299,
      416,  418,  268,    0,  251,    0,  217,  209,  859,  192,
        0,  192,  186,  435,  153,  146,  123,  859,   31,    0,
        0,  859,  859,  449,  459,  469,  472,  475,  485,  495,

      499,  509,  513,  515,  523,  531,  535,  545,  549,  555
    ) ;

yy_def : constant array(0..610) of short :=
    (   0,
      593,    1,    1,    2,    1,    2,    1,    2,  594,  594,
      595,  595,  593,  593,  593,  593,  593,  596,  597,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  598,  593,  593,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      593,  593,  593,  593,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  593,  593,  593,  599,  593,
      593,  593,  600,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,

      593,  593,  593,  593,  596,  593,  596,  601,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  602,  593,  603,
      593,  593,  593,  593,  604,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  593,  593,  593,  593,  605,  593,  593,  593,  593,

      593,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  593,  593,  599,   80,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  601,  593,
      593,  602,  606,  593,  593,  593,  593,  607,  608,  593,
      593,  593,  593,  593,  593,  593,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  593,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,

      598,  598,  598,  598,  598,  598,  598,  593,  593,  593,
      593,  593,  593,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  609,  606,  606,  593,  593,  593,
      593,  593,  593,  593,  607,  607,  608,  593,  598,  593,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,

      598,  598,  598,  593,  593,  593,  593,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  610,  593,  593,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  598,  598,  593,  598,
      598,  598,  598,  598,  598,  598,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  610,
      610,  593,  593,  598,  598,  598,  598,  598,  598,  598,

      598,  598,  598,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  593,  598,  598,  598,
      598,  598,  598,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  598,  598,  598,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  593,  598,  598,  593,  598,
      593,  593,  593,  593,  593,  593,  598,  598,  598,  598,
      598,  598,  598,  598,  598,  598,  593,  593,  593,  593,
      593,  593,  598,  598,  598,  598,  598,  593,  593,  593,
      598,  598,  598,  593,  593,  598,  598,  593,  593,  598,
      598,  593,    0,  593,  593,  593,  593,  593,  593,  593,

      593,  593,  593,  593,  593,  593,  593,  593,  593,  593
    ) ;

yy_nxt : constant array(0..920) of short :=
    (   0,
       14,   15,   16,   17,   18,   19,   20,   14,   21,   22,
       23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
       33,   34,   35,   36,   37,   37,   37,   37,   37,   38,
       14,   39,   14,   40,   41,   42,   43,   44,   45,   46,
       37,   47,   37,   48,   49,   50,   51,   52,   53,   54,
       55,   56,   57,   58,   59,   60,   37,   37,   61,   62,
       63,   64,   80,  101,  102,   82,  112,  592,   79,  110,
      140,  141,  106,   83,  106,  114,  113,  103,   82,  118,
      111,  150,  272,  115,  116,  119,   83,  137,  138,  139,
      147,  151,  273,   65,  152,   66,  148,   67,   68,  166,

      107,   69,  149,  187,   84,   70,   71,   72,   85,  184,
       73,   74,  167,   75,   86,  432,  188,   84,  158,   76,
       77,   85,   87,  159,  132,  185,  432,   86,  172,  160,
      133,  134,  189,  190,  120,   87,   88,  173,   89,  174,
       90,   91,  193,  121,   92,  122,  122,  150,   93,   94,
       95,  127,  128,   96,   97,  124,   98,  203,  213,  591,
      152,  126,   99,  100,  120,  129,  124,  154,  130,  135,
      136,  131,  143,  121,  185,  122,  122,  155,  175,  156,
      194,  144,  590,  145,  123,  124,  169,  125,  146,  157,
      589,  126,  162,  123,  176,  170,  124,  177,  163,  164,

      180,  171,  195,  102,  165,  181,  202,  158,  182,  154,
      183,  196,  159,  193,  125,  144,  103,  145,  205,  155,
      162,  204,  146,  587,  208,  172,  206,  207,  175,  586,
      180,  157,  165,  170,  209,  211,  174,  187,  182,  171,
      212,  489,  197,  106,  176,  106,  198,  210,  199,  585,
      214,  215,  200,  229,  230,  101,  102,  269,  234,  584,
      201,  218,  102,  235,  244,  244,  270,  120,  491,  103,
      196,  107,  245,  245,  583,  103,  121,  372,  122,  122,
      246,  373,  246,  122,  122,  247,  247,  292,  124,  127,
      128,  252,  253,  293,  126,  195,  102,  344,  219,  124,

      294,  197,  295,  129,  196,  198,  345,  199,  582,  103,
      593,  200,  326,  326,  533,  216,  348,  348,  581,  201,
      351,  351,  247,  247,  347,  533,  349,  353,  353,  367,
      368,  381,  350,  344,  382,  197,  352,  349,  489,  198,
      580,  199,  345,  354,  593,  200,  405,  405,  326,  326,
      593,  381,  579,  201,  411,  326,  326,  420,  420,  593,
      347,  578,  348,  348,  434,  491,  434,  348,  348,  435,
      435,  593,  349,  351,  351,  351,  351,  593,  350,  353,
      353,  353,  353,  349,  326,  326,  477,  477,  577,  352,
      479,  479,  487,  576,  487,  354,  575,  488,  488,  435,

      435,  492,  492,  488,  488,  531,  531,  492,  492,  492,
      492,  549,  549,  531,  531,  531,  531,  493,  549,  549,
      555,  532,  555,  493,  574,  556,  556,  556,  556,  532,
      571,  571,  571,  571,  571,  571,  588,  588,  573,  570,
      569,  568,  567,  566,  565,  564,  572,  563,  572,   81,
       81,   81,   81,   81,   81,   81,   81,   81,   81,   82,
       82,   82,   82,   82,   82,   82,   82,   82,   82,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  108,
      108,  142,  142,  142,  142,  216,  216,  216,  562,  216,
      216,  216,  216,  216,  216,  220,  220,  220,  220,  561,

      220,  220,  220,  220,  220,  239,  239,  239,  239,  242,
      560,  242,  242,  242,  242,  242,  242,  242,  242,  243,
      243,  248,  248,  308,  308,  308,  308,  559,  308,  308,
      308,  308,  308,  346,  558,  557,  346,  346,  346,  554,
      346,  355,  355,  553,  355,  357,  357,  357,  357,  357,
      357,  357,  357,  357,  357,  433,  433,  490,  552,  551,
      550,  490,  490,  548,  490,  547,  546,  545,  544,  543,
      542,  541,  540,  539,  538,  537,  536,  535,  534,  530,
      529,  528,  527,  526,  525,  524,  523,  522,  521,  520,
      519,  518,  517,  516,  515,  514,  513,  512,  511,  510,

      509,  508,  507,  506,  505,  504,  503,  502,  501,  500,
      499,  498,  497,  496,  495,  494,  486,  485,  484,  483,
      482,  481,  480,  478,  476,  475,  474,  473,  472,  471,
      470,  313,  469,  468,  467,  466,  465,  464,  463,  462,
      461,  460,  459,  458,  457,  456,  455,  454,  453,  452,
      451,  450,  449,  448,  447,  446,  445,  444,  443,  442,
      441,  440,  439,  438,  437,  436,  128,  593,  356,  431,
      430,  429,  428,  427,  426,  425,  424,  423,  422,  421,
      419,  418,  417,  416,  415,  414,  413,  412,  410,  374,
      409,  408,  407,  406,  313,  404,  403,  402,  401,  400,

      399,  398,  397,  396,  395,  394,  393,  392,  391,  390,
      389,  388,  387,  386,  385,  384,  383,  380,  379,  378,
      377,  376,  375,  374,  371,  370,  369,  366,  365,  364,
      363,  362,  361,  360,  359,  358,  128,  356,  343,  342,
      341,  340,  339,  338,  337,  336,  335,  334,  333,  332,
      331,  330,  329,  328,  327,  325,  324,  323,  322,  321,
      286,  320,  319,  318,  317,  316,  315,  314,  313,  312,
      311,  310,  309,  307,  306,  305,  304,  303,  302,  301,
      300,  299,  298,  297,  296,  291,  290,  289,  288,  287,
      286,  285,  284,  283,  282,  281,  280,  279,  278,  277,

      276,  275,  274,  271,  268,  267,  266,  265,  264,  263,
      262,  261,  260,  259,  258,  257,  256,  255,  254,  251,
      250,  249,  241,  240,  593,  118,  238,  237,  236,  233,
      232,  231,  228,  227,  226,  225,  224,  223,  222,  221,
      217,  192,  191,  186,  179,  178,  168,  161,  153,  117,
      109,  104,  593,   79,   79,   79,   78,   78,   13,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,

      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593
    ) ;

yy_chk : constant array(0..920) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    8,   15,   15,    9,   24,  589,    8,   23,
       35,   35,   18,    9,   18,   26,   24,   15,   10,   28,
       23,   42,  157,   26,   26,   28,   10,   34,   34,   34,
       41,   42,  157,    2,   42,    2,   41,    2,    2,   48,

       18,    2,   41,   58,    9,    2,    2,    2,    9,   56,
        2,    2,   48,    2,    9,  344,   58,   10,   45,    2,
        2,   10,    9,   45,   33,   56,  344,   10,   51,   45,
       33,   33,   59,   59,   30,   10,   12,   51,   12,   51,
       12,   12,   62,   30,   12,   30,   30,   66,   12,   12,
       12,   31,   31,   12,   12,   30,   12,   66,   74,  587,
       66,   30,   12,   12,   29,   31,   30,   44,   31,   33,
       33,   31,   40,   29,   74,   29,   29,   44,   52,   44,
       62,   40,  586,   40,   29,   29,   50,   29,   40,   44,
      585,   29,   47,   29,   52,   50,   29,   52,   47,   47,

       55,   50,   64,   64,   47,   55,   65,   68,   55,   67,
       55,   64,   68,   76,   29,   65,   64,   65,   68,   67,
       69,   67,   65,  583,   70,   71,   69,   69,   72,  582,
       73,   67,   69,   70,   71,   73,   71,   75,   73,   70,
       73,  433,   64,  105,   72,  105,   64,   72,   64,  580,
       75,   76,   64,   92,   92,  101,  101,  155,   96,  578,
       64,   80,   80,   96,  121,  121,  155,  122,  433,  101,
       80,  105,  123,  123,  577,   80,  122,  270,  122,  122,
      124,  270,  124,  126,  126,  124,  124,  179,  122,  127,
      127,  134,  134,  179,  122,  195,  195,  243,   80,  122,

      179,   80,  179,  127,  195,   80,  243,   80,  575,  195,
      217,   80,  215,  215,  489,  217,  244,  244,  573,   80,
      245,  245,  246,  246,  243,  489,  244,  247,  247,  266,
      266,  278,  244,  346,  278,  195,  245,  244,  490,  195,
      570,  195,  346,  247,  491,  195,  309,  309,  313,  313,
      347,  318,  569,  195,  318,  320,  320,  330,  330,  347,
      346,  567,  348,  348,  349,  490,  349,  350,  350,  349,
      349,  491,  348,  351,  351,  352,  352,  347,  348,  353,
      353,  354,  354,  348,  416,  416,  418,  418,  566,  351,
      421,  421,  432,  565,  432,  353,  563,  432,  432,  434,

      434,  435,  435,  487,  487,  488,  488,  492,  492,  493,
      493,  521,  521,  531,  531,  532,  532,  435,  546,  546,
      533,  488,  533,  492,  560,  533,  533,  555,  555,  531,
      556,  556,  571,  571,  572,  572,  584,  584,  559,  554,
      553,  552,  551,  550,  548,  547,  556,  545,  571,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  595,
      595,  595,  595,  595,  595,  595,  595,  595,  595,  596,
      596,  596,  596,  596,  596,  596,  596,  596,  596,  597,
      597,  598,  598,  598,  598,  599,  599,  599,  542,  599,
      599,  599,  599,  599,  599,  600,  600,  600,  600,  541,

      600,  600,  600,  600,  600,  601,  601,  601,  601,  602,
      540,  602,  602,  602,  602,  602,  602,  602,  602,  603,
      603,  604,  604,  605,  605,  605,  605,  539,  605,  605,
      605,  605,  605,  606,  535,  534,  606,  606,  606,  530,
      606,  607,  607,  527,  607,  608,  608,  608,  608,  608,
      608,  608,  608,  608,  608,  609,  609,  610,  525,  524,
      523,  610,  610,  520,  610,  518,  517,  516,  513,  509,
      508,  507,  504,  502,  500,  499,  497,  496,  494,  486,
      485,  484,  483,  482,  480,  478,  476,  475,  474,  473,
      472,  470,  469,  466,  465,  464,  463,  462,  461,  460,

      459,  458,  457,  456,  455,  454,  453,  452,  451,  450,
      448,  445,  444,  443,  437,  436,  429,  428,  427,  426,
      425,  423,  422,  419,  415,  414,  413,  412,  411,  409,
      408,  407,  406,  402,  400,  397,  396,  395,  393,  392,
      391,  390,  389,  388,  387,  385,  384,  383,  382,  381,
      380,  378,  377,  376,  374,  373,  372,  369,  368,  367,
      366,  364,  363,  362,  361,  359,  357,  356,  355,  343,
      342,  341,  340,  339,  337,  336,  335,  333,  332,  331,
      329,  328,  325,  324,  323,  322,  321,  319,  317,  316,
      315,  314,  312,  311,  310,  307,  306,  305,  304,  302,

      301,  300,  299,  298,  297,  296,  295,  294,  291,  290,
      289,  288,  286,  285,  281,  280,  279,  277,  276,  275,
      274,  273,  272,  271,  269,  268,  267,  265,  264,  263,
      262,  261,  260,  259,  257,  250,  249,  248,  237,  236,
      235,  234,  233,  231,  230,  229,  228,  227,  226,  225,
      223,  222,  221,  219,  216,  214,  213,  212,  211,  210,
      209,  208,  207,  206,  205,  204,  203,  202,  201,  200,
      199,  198,  197,  191,  190,  189,  188,  187,  186,  185,
      184,  183,  182,  181,  180,  178,  177,  176,  175,  174,
      173,  171,  170,  169,  168,  167,  166,  164,  163,  161,

      160,  159,  158,  156,  154,  153,  152,  151,  150,  149,
      148,  147,  146,  145,  144,  143,  141,  137,  135,  133,
      132,  129,  117,  110,  107,  103,   99,   98,   97,   95,
       94,   93,   91,   90,   89,   88,   87,   86,   85,   84,
       79,   61,   60,   57,   54,   53,   49,   46,   43,   27,
       20,   17,   13,    7,    6,    5,    4,    3,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,

      593,  593,  593,  593,  593,  593,  593,  593,  593,  593,
      593,  593,  593,  593,  593,  593,  593,  593,  593,  593
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
	    if ( yy_current_state >= 594 ) then
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
		    if ( yy_current_state >= 594 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 593 ) then
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
--# line 53 "javallel_lex.l"

        -- ignore "is" at start of line as far as indenting
        ECHO_L; 


when 2 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 58 "javallel_lex.l"

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
        Public_Length : constant := 6; --  "public"'Length
    begin
        pragma Assert (Col_Count = 0);

        --  Start of line, determine indent
        ECHO_L(YYT(YYT'First .. YYT'Last - Public_Length)); 

        --  Put "public" back
        for I in reverse YYT'Last - Public_Length + 1 .. YYT'Last loop
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
           yylval := Create_Token(Text => "");
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
--# line 145 "javallel_lex.l"

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

        if not Expecting_Indent then
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
    end;


when 4 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 2;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 176 "javallel_lex.l"

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
--# line 194 "javallel_lex.l"

        -- ignore "for" after "implements" at start of line as far as indenting
        ECHO_L; 
        ENTER(CHARLIT);


when 6 => 
--# line 200 "javallel_lex.l"

    declare
        -- ignore spaces,Carriage returns,tabs,form feeds
        -- 
        -- determine indent (expanding tabs appropriately)
        -- compare to top of indent "stack"
        -- if less, emit OUTDENTs while popping from stack until find match
        --   (except ignore such a line if it starts with 
        --     "then"/"new"/"implements"/"public"/"||"/"*XXX*")
        --   * complain if new level does not match one already on stack
        -- if same, emit NEWLINE; leave stack as is.
        -- if indent is more than top of stack:
        --   * if prior line ends with ":", push indent on stack and emit INDENT
        --   * if prior line does *not* end with ":" do not push indent on stack

        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        if YYT(YYT'First) > ' ' then
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
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
        if Debug_Indent and then Expecting_Indent then
           Text_IO.Put(" [NL: indent off] ");
        end if;
        Expecting_Indent := False;
    end;


when 7 => 
--# line 291 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 8 => 
--# line 297 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 9 => 
--# line 303 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 10 => 
--# line 309 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 11 => 
--# line 315 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 12 => 
--# line 321 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 13 => 
--# line 327 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OP_kw);


when 14 => 
--# line 333 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (STATIC_kw);


when 15 => 
--# line 339 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 16 => 
--# line 345 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VOID_kw);


when 17 => 
--# line 351 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (R_BRACE);


  -- Javallel reserved words
when 18 => 
--# line 358 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABS_kw);


when 19 => 
--# line 363 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 20 => 
--# line 368 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALL_kw);


when 21 => 
--# line 373 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AND_kw);


when 22 => 
--# line 378 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AS_kw);


when 23 => 
--# line 383 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ASSERT_kw);


when 24 => 
--# line 388 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BEGIN_kw);


when 25 => 
--# line 393 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := BLOCK_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (BLOCK_kw);


when 26 => 
--# line 403 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BREAK_kw);


when 27 => 
--# line 408 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CASE_kw);


when 28 => 
--# line 413 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 29 => 
--# line 418 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONST_kw);


when 30 => 
--# line 423 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONTINUE_kw);


when 31 => 
--# line 428 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEFAULT_kw);


when 32 => 
--# line 433 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EACH_kw);


when 33 => 
--# line 438 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := ELSE_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (ELSE_kw);


when 34 => 
--# line 448 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""else if"" rather than ""elseif""");
	return (ELSIF_kw);


when 35 => 
--# line 454 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
        --  Allow "elif" as an alias for "elsif" for Python-like syntax
	return (ELSIF_kw);


when 36 => 
--# line 460 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ELSIF_kw);


when 37 => 
--# line 465 "javallel_lex.l"

        unput('f'); unput('i');
	ECHO_L("end"); ENTER(CHARLIT); 
        yylval := Create_Token("end");
	yyerror("Use ""end if"" rather than ""endif""");
	return (END_kw);


when 38 => 
--# line 472 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 39 => 
--# line 477 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXIT_kw);


when 40 => 
--# line 482 "javallel_lex.l"

        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (PUBLIC_kw);
        end;


when 41 => 
--# line 495 "javallel_lex.l"

        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (PUBLIC_kw);
        end;


when 42 => 
--# line 508 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXTENDS_kw);


when 43 => 
--# line 513 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FOR_kw);


when 44 => 
--# line 518 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FORWARD_kw);


when 45 => 
--# line 523 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 46 => 
--# line 528 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FINAL_kw);


when 47 => 
--# line 533 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GLOBAL_kw);


when 48 => 
--# line 538 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IF_kw);


when 49 => 
--# line 543 "javallel_lex.l"

	ECHO_L; ENTER(AFTER_IMPLEMENTS); 
        --  TBD: We might want to return a NEWLINE first.
	yylval := Create_Token;
	return (IMPLEMENTS_kw);


when 50 => 
--# line 549 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 51 => 
--# line 554 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IN_kw);


when 52 => 
--# line 559 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 53 => 
--# line 564 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := IS_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (IS_kw);


when 54 => 
--# line 574 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LAMBDA_kw);


when 55 => 
--# line 579 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LOCKED_kw);


when 56 => 
--# line 584 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (MOD_kw);


when 57 => 
--# line 589 "javallel_lex.l"

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


when 58 => 
--# line 603 "javallel_lex.l"

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


when 59 => 
--# line 617 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NOT_kw);


when 60 => 
--# line 622 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NULL_kw);


when 61 => 
--# line 627 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := OF_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (OF_kw);


when 62 => 
--# line 637 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OP_kw);


when 63 => 
--# line 642 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPT_kw);


when 64 => 
--# line 647 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPTIONAL_kw);


when 65 => 
--# line 652 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OR_kw);


when 66 => 
--# line 657 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 67 => 
--# line 662 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRIVATE_kw);


when 68 => 
--# line 667 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (QUEUED_kw);


when 69 => 
--# line 672 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REF_kw);


when 70 => 
--# line 677 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REM_kw);


when 71 => 
--# line 682 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RETURN_kw);


when 72 => 
--# line 687 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REVERSE_kw);


when 73 => 
--# line 692 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SOME_kw);


when 74 => 
--# line 697 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (STATIC_kw);


when 75 => 
--# line 702 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SWITCH_kw);


when 76 => 
--# line 707 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 77 => 
--# line 712 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 78 => 
--# line 722 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 79 => 
--# line 732 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TYPE_kw);


when 80 => 
--# line 737 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (UNTIL_kw);


when 81 => 
--# line 742 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VAR_kw);


when 82 => 
--# line 747 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VOID_kw);


when 83 => 
--# line 752 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WHILE_kw);


when 84 => 
--# line 757 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WITH_kw);


when 85 => 
--# line 762 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (XOR_kw);


  -- Match all the compound Javallel delimiters. 
when 86 => 
--# line 769 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMPARE);


when 87 => 
--# line 774 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(EQ);


when 88 => 
--# line 779 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(NEQ);


when 89 => 
--# line 784 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GEQ);


when 90 => 
--# line 789 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LEQ);


when 91 => 
--# line 794 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT);


when 92 => 
--# line 799 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER);


when 93 => 
--# line 804 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(ASSIGN);


when 94 => 
--# line 809 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MOVE);


when 95 => 
--# line 814 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(SWAP);


when 96 => 
--# line 819 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOT_DOT);


when 97 => 
--# line 824 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_INTERVAL);


when 98 => 
--# line 829 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_CLOSED_INTERVAL);


when 99 => 
--# line 834 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(CLOSED_OPEN_INTERVAL);


when 100 => 
--# line 839 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOUBLE_COLON);


when 101 => 
--# line 844 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);


when 102 => 
--# line 849 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GIVES);


when 103 => 
--# line 854 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(IMPLIES);


when 104 => 
--# line 859 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 105 => 
--# line 864 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 106 => 
--# line 869 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_ASSIGN);


when 107 => 
--# line 874 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_ASSIGN);


when 108 => 
--# line 879 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(TIMES_ASSIGN);


when 109 => 
--# line 884 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DIVIDE_ASSIGN);


when 110 => 
--# line 889 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER_ASSIGN);


when 111 => 
--# line 894 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_ASSIGN);


when 112 => 
--# line 899 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_MOVE);


when 113 => 
--# line 904 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_ASSIGN);


when 114 => 
--# line 909 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_ASSIGN);


when 115 => 
--# line 914 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(XOR_ASSIGN);


when 116 => 
--# line 919 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT_ASSIGN);


when 117 => 
--# line 924 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(RSHIFT_ASSIGN);


when 118 => 
--# line 929 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_OR);


when 119 => 
--# line 934 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_AND);


when 120 => 
--# line 939 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_PLUS);


when 121 => 
--# line 944 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_MINUS);


  -- Match all the Javallel single-character delimiters.
when 122 => 
--# line 951 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(PRIME);


when 123 => 
--# line 956 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('(');


when 124 => 
--# line 961 "javallel_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(')');


when 125 => 
--# line 966 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('[');


when 126 => 
--# line 971 "javallel_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(']');


when 127 => 
--# line 976 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('<');


when 128 => 
--# line 981 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('>');


when 129 => 
--# line 986 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
        Bracketing_Token := L_BRACE;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(L_BRACE);


when 130 => 
--# line 996 "javallel_lex.l"
  --  Use > ... < for assertions in Javallel
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);


when 131 => 
--# line 1001 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_BRACE);


when 132 => 
--# line 1006 "javallel_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);


when 133 => 
--# line 1011 "javallel_lex.l"

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


when 134 => 
--# line 1031 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('+');


when 135 => 
--# line 1036 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(',');


when 136 => 
--# line 1041 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('-');


when 137 => 
--# line 1046 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('.');


when 138 => 
--# line 1051 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('/');


when 139 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1056 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
        Bracketing_Token := ':';
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(EOL_COLON);


when 140 => 
--# line 1066 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(':');


when 141 => 
--# line 1071 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(';');


when 142 => 
--# line 1076 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('|');


when 143 => 
--# line 1081 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('&');


when 144 => 
--# line 1086 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('?');


when 145 => 
--# line 1091 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');


  -- The following is used to match all valid Javallel identifiers
  -- except reserved words. Note that leading digits and underscores
  -- are not allowed and that double underscores are not allowed.
when 146 => 
--# line 1101 "javallel_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);


  -- Enumeration literals
when 147 => 
--# line 1108 "javallel_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Enum_Literal);


  -- Decimal numeric literals
when 148 => 
--# line 1115 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 149 => 
--# line 1121 "javallel_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


  -- Based numeric literals.
when 150 => 
--# line 1129 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 151 => 
--# line 1135 "javallel_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


when 152 => 
--# line 1141 "javallel_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


when 153 => 
--# line 1147 "javallel_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


  -- Match all valid character literals.  See Ada LRM 2.6.
when 154 => 
--# line 1155 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Char_Literal);


  -- Match all valid string literals.  See Ada LRM 2.6.
when 155 => 
--# line 1163 "javallel_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        declare
           Tok : constant String := yytext;
           Quote : constant String := """";
        begin
           yylval := Create_Token (Tok);
           if Tok (Tok'Last) /= Quote(1) then
              yyerror ("unterminated string", At_Token => yylval);
           end if;
           return(String_Literal);
        end;


  -- Handle white space --
when 156 => 
--# line 1179 "javallel_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 157 => 
--# line 1181 "javallel_lex.l"

        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;


when 158 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1188 "javallel_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 159 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1198 "javallel_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 160 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1208 "javallel_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 161 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1218 "javallel_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 162 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1228 "javallel_lex.l"

        --  Ignore outdent for label
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 163 => 
--# line 1238 "javallel_lex.l"

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
--# line 1326 "javallel_lex.l"

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
--# line 1363 "javallel_lex.l"

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
when 166 => 
--# line 1392 "javallel_lex.l"

        --  TBD: Should we return NEWLINE if this line is entirely blank?
        if Echo_Input then
           text_io.new_line;
        end if;
        Display_Linenum;


  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.
when 167 => 
--# line 1403 "javallel_lex.l"
ECHO_L; 
	    yyerror("lexical error: '" &
	      javallel_lex_dfa.yytext & "'");


when 168 => 
--# line 1408 "javallel_lex.l"
raise AFLEX_SCANNER_JAMMED;
when YY_END_OF_BUFFER + RESCANNING + 1 => 
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
--# line 1408 "javallel_lex.l"

end javallel_lex;


