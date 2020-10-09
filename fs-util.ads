-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                           F S . U T I L                             --
--                                                                     --
--                     S p e c i f i c a t i o n                       --
--                                                                     --
--                         $ Revision: 1.0 $                           --
--                                                                     --
--       Alejandro Villanueva Uribarri (Universidad de Zaragoza)       --
--                                                                     --
-------------------------------------------------------------------------
-------------------------------------------------------------------------
--                                                                     --
-------------------------------------------------------------------------


   with fs.fs;
   use  fs.fs;

   with Ada.Strings.Unbounded;
   use  Ada.Strings.Unbounded;

   package fs.util is
   
      procedure Get_BCD (The_File:   in out File_Handler;
                         Sector:     in out DWord;
                         The_Byte:   in     Byte;
                         Where:      in out Integer;
                         The_Block:     out Data_Block;
                         The_Number:    out Natural;
                         Found:         out Boolean);
      procedure Get_String (The_File:   in out File_Handler;
                            Sector:     in out DWord;
                            The_Byte:   in     Byte;
                            Where:      in out Integer;
                            The_Block:     out Data_Block;
                            The_String:    out Unbounded_String;
                            Found:         out Boolean);
      procedure Find_Byte (The_File:  in out File_Handler;
                           Sector:    in out DWord;
                           The_Byte:  in     Byte;
                           Where:     in out Integer;
                           The_Block:    out Data_Block;
                           Found:        out Boolean);
   
   end fs.util;

