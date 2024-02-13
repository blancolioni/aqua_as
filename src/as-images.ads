package As.Images is

   function Hex_Image (W : Word_8) return String;
   function Hex_Image (W : Word_16) return String;
   function Hex_Image (W : Word_32) return String;

private

   Hex_Digit  : constant String := "0123456789ABCDEF";

   function Hex_Image (W : Word_8) return String is
     (Hex_Digit (Natural (W) / 16 + 1),
      Hex_Digit (Natural (W) mod 16 + 1));

   function Hex_Image (W : Word_16) return String
   is (Hex_Image (Word_8 (W / 256))
       & Hex_Image (Word_8 (W mod 256)));

   function Hex_Image (W : Word_32) return String
   is (Hex_Image (Word_16 (W / 65536))
       & Hex_Image (Word_16 (W mod 65536)));

end As.Images;
