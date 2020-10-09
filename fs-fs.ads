-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                             F S . F S                               --
--                                                                     --
--                     S p e c i f i c a t i o n                       --
--                                                                     --
--                         $ Revision: 1.0 $                           --
--                                                                     --
--       Alejandro Villanueva Uribarri (Universidad de Zaragoza)       --
--                                                                     --
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- This package provides the Filesystem on top of the IDE driver. It's --
-- a simple filesystem that only  support read operations  on a FAT 16 --
-- partition.                                                          --
--                                                                     --
-- From here you get two protected types:                              --
-- (*) Path: with this you can create different Path Contexts that can --
--     be used by the File_Handler type to identify files on the disk. --
-- (*) File_Handler: variables of this type contain access information --
--     for the opened files in the system.                             --
-- We also get a default variable for each type:                       --
-- (*) Default_Path: Path;                                             --
-- (*) Default_File: File_Handler;                                     --
--                                                                     --
-- These types work  as follows:  To open a  file in our FAT 16 system --
-- named "C:\alex\pfc\pfc.txt" we proceed this way...                  --
-- (1) We assign the Path variable Default_Path to  "C:\alex\pfc" with --
--     the following operations:                                       --
--          Default_Path.cd_partition (0);                             --
--          Default_Path.cd_absolute (Create_File_Name ("alex"));      --
--          Default_Path.cd_relative (Create_File_Name ("pfc"));       --
-- (2) Now we open the file "pfc.txt" by issuing this command:         --
--          Default_File.Open (Create_File_Name ("pfc.txt"),           --
--                             Default_Path,                           --
--                             Read_Only);                             --
--          Please note that the last two parameters are optional  and --
--          that they default to Default_Path and Read_Only.           --
-- (3) We could have defined our own Path and File_Handler  variables. --
-- (4) If we want to print data from Default_File  upon screen we  use --
--     the Read_Sector operation like this:                            --
--          Data:       fs.Data_Block;                                 --
--          EOF, Final: Natural;                                       --
--          Sector:     fs.DWord := 0;                                 --
--          loop                                                       --
--             Default_File.Read_Sector (Data, EOF, Sector);           --
--             if EOF = 512 then Final := 511;                         --
--                          else Final := EOF;                         --
--             end if;                                                 --
--             for I in 0 .. Final loop                                --
--                Ada.Text_IO.Put (Character'Val (Data (I)));          --
--             end loop;                                               --
--             exit when Final = EOF;                                  --
--             Sector := Sector + 1;                                   --
--          end loop;                                                  --
--                                                                     --
-- Please note that only the Read operation is currently  implemented. --
-- Writing operation is not needed, so I won't implement it. Anyhow it --
-- is simple to implement in case you need to.                         --
-------------------------------------------------------------------------


   with Ada.Strings.Bounded;
   use  Ada.Strings.Bounded;

   package fs.fs is
   
      package Bounded_12 is new Generic_Bounded_Length (12);
   
      type File is new Bounded_12.Bounded_String;
      type File_Type is (Regular, Directory, Not_Found);
      type File_Mode is (Read_Only, Read_Write, Closed);
   
      function Create_File_Name (File_Name: in String)
      return File;
   
      protected type Path is
      
         function Get_First_Cluster_of_Dir
         return Word;
         function Get_Partition_Nr
         return Natural;
         procedure cd_absolute (New_Path: in File);
         procedure cd_relative (New_Path: in File);
         procedure cd_partition (New_Partition: in Natural);
      
      private
      
         Partition_Nr:         Natural := 0;
         First_Cluster_of_Dir: Word    := 0;
      
      end Path;
      Default_Path: Path;
   
      protected type File_Handler is
      
         entry Open (File_Name:    in File;
                     Path_Context: in Path      := Default_Path;
                     Open_Mode:    in File_Mode := Read_Only);
         entry Read_Sector (Where:          out Data_Block;
                            EOF_Mark_At:    out Natural;
                            Sector:      in     DWord      := 0);
         entry Close;
         function Is_Closed
         return Boolean;
      
      private
      
         Partition_Nr:          Natural   := 0;
         First_Cluster_of_File: Word      := 0;
         File_Size:             DWord     := 0;
         Mode:                  File_Mode := Closed;
         Cursor_Position:       DWord     := 0;
      
      end File_Handler;
      Default_File: File_Handler;
   
      File_Not_Found_Exception:     exception;
      End_Of_File_Exception:        exception;
      Invalid_Partition_Exception:  exception;
      Bad_Cluster_Number_Exception: exception;
   
   end fs.fs;

