-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                                F S                                  --
--                                                                     --
--                     S p e c i f i c a t i o n                       --
--                                                                     --
--                         $ Revision: 1.0 $                           --
--                                                                     --
--       Alejandro Villanueva Uribarri (Universidad de Zaragoza)       --
--                                                                     --
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- This file contains basic type declarations needed by the IDE driver --
-- and the File System.                                                --
-------------------------------------------------------------------------


   package fs is
   pragma Pure;
   
   --[ Types and constants ]---------------------------------------------
      type Byte is range 0 .. 2**8 - 1;
      for Byte'Size use 8;
   
      type Word is range 0 .. 2**16 - 1;
      for Word'Size use 16;
   
      type DWord is range 0 .. 2**32 - 1;
      for DWord'Size use 32;
   
      type Byte_Range is range Byte'First .. Byte'Last;
   
      Sector_Size: constant :=  512;
   
      type Data_Block is array (0 .. Sector_Size - 1) of Byte;
   
      type Read_or_Write is (Read, Write);
   
   ---[ Conversion Routines ]--------------------------------------------
      function To_Byte (Data: in Data_Block;
                        Pos:  in Word)
      return Byte;
      function To_Word (Data: in Data_Block;
                        Pos:  in Word)
      return Word;
      function To_DWord (Data: in Data_Block;
                         Pos:  in Word)
      return DWord;
   
   end fs;

