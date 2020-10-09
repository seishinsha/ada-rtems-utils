-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                            F S . I D E                              --
--                                                                     --
--                              B o d y                                --
--                                                                     --
--                         $ Revision: 1.0 $                           --
--                                                                     --
--       Alejandro Villanueva Uribarri (Universidad de Zaragoza)       --
--                                                                     --
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- This package provides the IDE_Driver. You can basically perform two --
-- operations with it:                                                 --
-- (*) IDE_Driver.Init will reset the driver.  This function is called --
--     when the package is withed.                                     --
-- (*) IDE_Driver.DEV_IO will read or write any sector into a variable --
--     of type Data_Block.                                             --
--                                                                     --
-- Please note that only the Read operation is currently  implemented. --
-- Writing operation is not needed, so I won't implement it. Anyhow it --
-- is simple to implement in case you need to.                         --
--                                                                     --
-- For this driver to work properly you should configure your disk  in --
-- CHS mode in the BIOS parameter table.  If configured in LBA mode it --
-- won't work.                                                         --
-------------------------------------------------------------------------


   with System.Storage_Elements, Port_IO, Ada.Text_IO; 
   use  System.Storage_Elements, Port_IO, Ada.Text_IO;

   package body fs.ide is
   
   ---[ On or Off ]------------------------------------------------------
      type On_or_Off is (Off, On);
      for On_or_Off use (Off => 0, On => 1);
      for On_or_Off'Size use 1;
   
   ---[ Interrupt Vector ]-----------------------------------------------
      type Interrupt_Vector is
         record
            Segment: Word;
            Offset:  Word;
         end record;
      for Interrupt_Vector use
      record
         Segment at 2 range 0 .. 15;
         Offset  at 0 range 0 .. 15;
      end record;
      Int_41h_Vector: Interrupt_Vector;
      for Int_41h_Vector'Address use To_Address (16#41# * 4);
   
   ---[ Drive Info ]-----------------------------------------------------
      type Drive_Info is
         record
            Cylinders: Word;
            Heads:     Byte;
            Sectors:   Byte;
         end record;
      for Drive_Info use
      record
         Cylinders at  0 range 0 .. 15;
         Heads     at  2 range 0 ..  7;
         Sectors   at 14 range 0 ..  7;
      end record;
      Info_hd0: Drive_Info;
      for Info_hd0'Address use To_Address
         (Integer_Address
             (Integer (Int_41h_Vector.Segment) * 16 +
              Integer (Int_41h_Vector.Offset) ) );
   
   ---[ I/O Ports ]------------------------------------------------------
      Reg_Data:     constant := 16#1F0#;
      Reg_Error:    constant := 16#1F1#;
      Reg_Sec_Cnt:  constant := 16#1F2#;
      Reg_Sec_Num:  constant := 16#1F3#;
      Reg_Cyl_LSB:  constant := 16#1F4#;
      Reg_Cyl_MSB:  constant := 16#1F5#;
      Reg_Drv_Head: constant := 16#1F6#;
      Reg_Status:   constant := 16#1F7#;
      Reg_Command:  constant := 16#1F7#;
   
   ---[ Status Register ]------------------------------------------------
      BSY: constant := 7;
      DRQ: constant := 3;
      ERR: constant := 0;
   
   ---[ Error Register ]-------------------------------------------------
      ABT: constant := 2;
   
   ---[IDE Driver ]------------------------------------------------------
      protected body IDE_Driver is
      
         function Byte_Status (The_Byte:  in Port_IO.Byte;
                               The_Bit:   in Byte_Range;
                               Check_For: in On_or_Off)
         return Boolean is
            Working_Byte: Port_IO.Byte := The_Byte;
            Remainder:    Port_IO.Byte;
         begin
            for I in 0 .. The_Bit loop
               Remainder := Working_Byte rem 2;
               Working_Byte := Working_Byte / 2;
            end loop;
            return (Remainder = On_or_Off'Pos (Check_For));
         end Byte_Status;
      
         procedure Recalibrate is
            Status: Port_IO.Byte;
         begin
            loop
               Inport_Byte (Reg_Status, Status);
               exit when Byte_Status (Status, BSY, Off);
            end loop;
            Outport_Byte (Reg_Drv_Head, 16#A0#);
            Outport_Byte (Reg_Command,  16#10#);
            loop
               Inport_Byte (Reg_Status, Status);
               exit when Byte_Status (Status, BSY, Off);
            end loop;
         end Recalibrate;
      
         entry Init when not Initialized is
            package Byte_IO is new Integer_IO (Byte);
            package Word_IO is new Integer_IO (Word);
            use Byte_IO, Word_IO;
         begin
            Put ("Hard Disk (hd0): ");
            Put (Info_hd0.Cylinders, Width => 0);
            Put (" cylinders, ");
            Put (Info_hd0.Heads,     Width => 0);
            Put (" heads, ");
            Put (Info_hd0.Sectors,   Width => 0);
            Put (" sectors");
            New_Line;
            Recalibrate;
            Initialized := True;
         end Init;
      
         entry Dev_IO (rw_flag: in     Read_or_Write;
                       Pos:     in     DWord;
                       Data:       out Data_Block)
         when Initialized is
         
            Ok:                             Boolean       := False;
            Error_Cnt:                      Byte          := 0;
            Max_Errors:                     constant Byte := 3;
         
            Cylinder, Remainder:            DWord;
            Cyl_LSB, Cyl_MSB, Head, Sector: Port_IO.Byte;
            Command, Status, Error:         Port_IO.Byte;
            Data_Mini_Block:                Port_IO.Word;
         
         begin
         
         -- get CHS format from Pos parameter
            Cylinder  :=
               Pos  /  DWord (Info_hd0.Sectors * Info_hd0.Heads);
            Remainder :=
               Pos rem DWord (Info_hd0.Sectors * Info_hd0.Heads);
            Cyl_MSB   :=
               Port_IO.Byte (Cylinder  /  256);
            Cyl_LSB   :=
               Port_IO.Byte (Cylinder rem 256);
            Head      :=
               Port_IO.Byte (Remainder  / 
                             DWord (Info_hd0.Sectors));
            Sector    :=
               Port_IO.Byte (Remainder rem
                             DWord (Info_hd0.Sectors)) + 1;
         
            if rw_flag = Read then 
               Command := 16#20#;
            else 
               Command := 16#30#;
            end if;
         
            while not Ok and Error_Cnt < Max_Errors loop
            
            -- wait until not BSY
               loop
                  Inport_Byte (Reg_Status, Status);
                  exit when Byte_Status (Status, BSY, Off);
               end loop;
            
            -- issue command
               Outport_Byte (Reg_Sec_Cnt,  1            );
               Outport_Byte (Reg_Sec_Num,  Sector       );
               Outport_Byte (Reg_Cyl_LSB,  Cyl_LSB      );
               Outport_Byte (Reg_Cyl_MSB,  Cyl_MSB      );
               Outport_Byte (Reg_Drv_Head, 16#A0# + Head);
               Outport_Byte (Reg_Command,  Command      );
            
            -- wait for data to be ready
               loop
                  Inport_Byte (Reg_Status, Status);
                  exit when
                     Byte_Status (Status, BSY, Off) and
                     Byte_Status (Status, DRQ, On);
               end loop;
            
            -- transfer data to main memory
               for I in 0 .. Sector_Size / 2 - 1 loop
                  Inport_Word (Reg_Data, Data_Mini_Block);
                  Data (I * 2 + 0) := Byte (Data_Mini_Block rem 256);
                  Data (I * 2 + 1) := Byte (Data_Mini_Block  /  256);
               end loop;
            
            -- check for errors
               Inport_Byte (Reg_Status, Status);
               Inport_Byte (Reg_Error,  Error );
               if  Byte_Status (Status, ERR, On )
               and Byte_Status (Status, BSY, Off)
               and Byte_Status (Error,  ABT, On ) then
               -- we've got a command abortion here!
                  Recalibrate;
                  Ok := False;
                  Error_Cnt := Error_Cnt + 1;
               else
                  Ok := True;
               end if;
            end loop;
         
            If Error_Cnt = Max_Errors then
               raise Disk_Error;
            end if;
         
         end Dev_IO;
      
      end IDE_Driver;
   
   begin
   
      IDE_Driver.Init;
   
   end fs.ide;

