package As is

   type Word_8 is mod 2 ** 8;
   type Word_16 is mod 2 ** 16;
   type Word_32 is mod 2 ** 32;

   type Register_Index is new Word_8;

   type Mention_Context is (No_Context, Relative_XY, Relative_XYZ);

end As;
