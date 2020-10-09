-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                             F S . F S                               --
--                                                                     --
--                              B o d y                                --
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

   package body fs.util is
   
      procedure Get_BCD (The_File:   in out File_Handler;
                         Sector:     in out DWord;
                         The_Byte:   in     Byte;
                         Where:      in out Integer;
                         The_Block:     out Data_Block;
                         The_Number:    out Natural;
                         Found:         out Boolean) is
      
         At_EOF: Natural;
         Final:  Natural;
         S:      DWord   := Sector;
         From:   Natural := Where + 1;
      
         Zero:        constant Natural := 48; -- '0'
      
      begin
      
         The_Number := 0;
         Found      := False;
      
      Main_Loop:
         loop
            The_File.Read_Sector (The_Block, At_EOF, S);
            if At_EOF = 512 then
               Final := 511;
            else
               Final := At_EOF;
            end if;
            for I in From .. Final loop
               if The_Block (I) = The_Byte then
                  Sector := S;
                  Where  := I;
                  Found  := True;
                  exit Main_Loop;
               else
                  The_Number := 10 * The_Number +
                                Integer (The_Block (I)) - Zero;
               end if;
            end loop;
            S    := S + 1;
            From := 0;
            exit when Final = At_EOF;
         end loop Main_Loop;
      
      end Get_BCD;
   
      procedure Get_String (The_File:   in out File_Handler;
                            Sector:     in out DWord;
                            The_Byte:   in     Byte;
                            Where:      in out Integer;
                            The_Block:     out Data_Block;
                            The_String:    out Unbounded_String;
                            Found:         out Boolean) is
      
         At_EOF: Natural;
         Final:  Natural;
         S:      DWord   := Sector;
         From:   Natural := Where + 1;
      
      begin
      
         The_String := To_Unbounded_String ("");
         Found      := False;
      
      Main_Loop:
         loop
            The_File.Read_Sector (The_Block, At_EOF, S);
            if At_EOF = 512 then
               Final := 511;
            else
               Final := At_EOF;
            end if;
            for I in From .. Final loop
               if The_Block (I) = The_Byte then
                  Sector := S;
                  Where  := I;
                  Found  := True;
                  exit Main_Loop;
               else
                  Append (The_String, Character'Val (The_Block (I)));
               end if;
            end loop;
            S    := S + 1;
            From := 0;
            exit when Final = At_EOF;
         end loop Main_Loop;
      
      end Get_String;
   
      procedure Find_Byte (The_File:  in out File_Handler;
                           Sector:    in out DWord;
                           The_Byte:  in     Byte;
                           Where:     in out Integer;
                           The_Block:    out Data_Block;
                           Found:        out Boolean) is
      
         At_EOF: Natural;
         Final:  Natural;
         S:      DWord   := Sector;
         From:   Natural := Where + 1;
      
      begin
      
         Found := False;
      
      Main_Loop:
         loop
            The_File.Read_Sector (The_Block, At_EOF, S);
            if At_EOF = 512 then
               Final := 511;
            else
               Final := At_EOF;
            end if;
            for I in From .. Final loop
               if The_Block (I) = The_Byte then
                  Sector := S;
                  Where  := I;
                  Found  := True;
                  exit Main_Loop;
               end if;
            end loop;
            S    := S + 1;
            From := 0;
            exit when Final = At_EOF;
         end loop Main_Loop;
      
      end Find_Byte;
   
   end fs.util;

