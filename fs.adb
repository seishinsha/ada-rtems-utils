-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                                F S                                  --
--                                                                     --
--                              B o d y                                --
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


   package body fs is
   
      function To_Byte (Data: in Data_Block;
                        Pos:  in Word)
      return Byte is
      begin
         return Data (Integer (Pos));
      end To_Byte;
   
      function To_Word (Data: in Data_Block;
                        Pos:  in Word)
      return Word is
      begin 
         return Word (Data (Integer (Pos))) +
            Word (Data (Integer (Pos + 1))) * 256;
      end To_Word;
   
      function To_DWord (Data: in Data_Block;
                         Pos:  in Word)
      return DWord is
      begin 
         return DWord (Data (Integer (Pos))) +
            DWord (Data (Integer (Pos + 1))) * 256 +
            DWord (Data (Integer (Pos + 2))) * 256**2 +
            DWord (Data (Integer (Pos + 3))) * 256**3;
      end To_DWord;
   
   end fs;

