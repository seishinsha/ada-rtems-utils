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


   with fs.ide, Ada.Text_IO, Ada.Characters.Handling; 
   use  fs.ide, Ada.Text_IO, Ada.Characters.Handling;

pragma Elaborate (fs.ide);

   package body fs.fs is
   
   ---[ Partitions ]-----------------------------------------------------
      Initialized: Boolean := False;
   
      type Partition_Structure is
         record
         -- map
            First_Sector:          DWord;            
            First_Root_Dir_Sector: DWord; -- relative to First_Sector
            First_Data_Sector:     DWord; -- relative to First_Sector
         -- sizes
            Size:                  DWord;
            Sectors_per_Cluster:   Byte;
            Reserved_Sectors:      Word;            
            Num_FATs:              Byte;
            FAT_Size:              Word;
         -- counters
            Root_Dir_Entries:      Word;
            Root_Dir_Sectors:      Word;
         -- status
            Valid_FAT_Volume:      Boolean;
         end record;
      type Partition_Table is array (0 .. 3) of Partition_Structure;
      Partition: Partition_Table;
   
   ---[ Partition Structures ]-------------------------------------------
      P: constant array (0 .. 3) of Word := (446, 462, 478, 494);
   
   ---[ Private Methods ]------------------------------------------------
      function FAT_Cluster_Entry_Value (Partition_Number: in Natural;
                                        N:                in Word)
      return Word is
      
         db:                     Data_Block;
         This_FAT_Sector_Number: DWord;
         This_FAT_Entry_Offset:  Word;
      
      begin
      
         This_FAT_Sector_Number :=
            Partition (Partition_Number).First_Sector +
            DWord (Partition (Partition_Number).Reserved_Sectors) +
            DWord (N) / 256;
         This_FAT_Entry_Offset  :=
            2 * N rem 256;
         IDE_Driver.Dev_IO (Read, This_FAT_Sector_Number, db);
         return To_Word (db, This_FAT_Entry_Offset);
      
      end FAT_Cluster_Entry_Value;
   
      function First_Sector_of_Cluster (Partition_Number: in Natural;
                                        N:                in Word)
      return DWord is
      
         M: constant Word := N - 2;  
      
      begin
      
         if M < 0 then 
            raise Bad_Cluster_Number_Exception;
         end if;
      
      -- returns the first sector of a specified data cluster inside
      -- a FAT16 partition, relative to the begining of the disk
         return
            DWord (M) *
            DWord (Partition (Partition_Number).Sectors_per_Cluster) +
            Partition (Partition_Number).First_Data_Sector +
            Partition (Partition_Number).First_Sector;
      
      end First_Sector_of_Cluster;
   
   ---[ Path ]-----------------------------------------------------------
      type FAT_Name is array (1 .. 11) of Byte;
   
      function To_FAT_Name (Name: in File)
      return FAT_Name is
      
         Result: FAT_Name := (others => Character'Pos (' '));
         I:      Natural  := 1;
         J:      Natural  := 1;
      
      begin
      
         while I <= Length (Name)
         and   J <= Result'Last   loop
            if Element (Name, I) = '.' then
               I := I + 1;
               J := 9; 
            end if;
            Result (J) :=
               Byte (Character'Pos (To_Upper (Element (Name, I))));
            I := I + 1; 
            J := J + 1;
         end loop;
         return Result;
      
      end To_FAT_Name;
   
      protected body Path is
      
         function Get_First_Cluster_of_Dir
         return Word is
         begin
            return First_Cluster_of_Dir;
         end Get_First_Cluster_of_Dir;
      
         function Get_Partition_Nr
         return Natural is
         begin
            return Partition_Nr;
         end Get_Partition_Nr;
      
         procedure cd_absolute (New_Path: in File) is
         
            db:        Data_Block;
            Found:     Boolean;
            I:         DWord      := 0;
            J:         Natural;
            Byte_Name: FAT_Name;
         
         begin 
         
            Byte_Name := To_FAT_Name (New_Path);
         
         Main_Loop:
         -- scan the root dir area for the path specified
            while I < DWord (Partition (Partition_Nr).Root_Dir_Sectors)
            loop
               IDE_Driver.Dev_IO
                     (Read,
                      Partition (Partition_Nr).First_Root_Dir_Sector +
                      Partition (Partition_Nr).First_Sector +
                      I,
                      db);   
            -- scan each and every dir entry in the sector
               J := 0;
               while J < 512 loop
                  Found := True;
                  if db (J) = 0 then
                  -- this entry is empty and it's the last one, so...
                     Found := False; 
                     exit Main_Loop;
                  end if;
                  if db (J) /= 16#E5# then
                  -- this entry contains valid data
                     if db (J) = 16#05# then
                     -- the actual db(J) should be 16#E5# so we change
                        db (J) := 16#E5#;
                     end if;
                     if (db (J + 11) / 16) rem 2 = 1 then
                     -- it's a directory
                        for Index in 1 .. 11 loop
                           if db (J + Index - 1) /= Byte_Name (Index)
                           then
                              Found := False;
                              exit;
                           end if;
                        end loop;
                     else
                     -- it's not a directory
                        Found := False;
                     end if;
                     exit Main_Loop when Found;
                  end if;
                  J := J + 32;
               end loop;
            
               I := I + 1;
            end loop Main_Loop;
         
            If Found then
               First_Cluster_of_Dir := To_Word (db, Word (J + 26));
            else 
               raise File_Not_Found_Exception;
            end if;
         
         end cd_absolute;
      
         procedure cd_relative (New_Path: in File) is
         
            db:        Data_Block;
            Found:     Boolean;
            I:         Byte;
            J:         Natural;
            K:         Word       := First_Cluster_of_Dir;
            Byte_Name: FAT_Name;
         
         begin 
         
            Byte_Name := To_FAT_Name (New_Path);
         
         Main_Loop:
         -- scan each cluster of the directory
            while K < 16#FFF8# loop
            -- scan every sector of every cluster
               I := 0;
               while I < Partition (Partition_Nr).Sectors_per_Cluster
               loop
                  IDE_Driver.Dev_IO (Read,
                                     First_Sector_of_Cluster
                                        (Partition_Nr, K) +
                                     DWord (I),
                                     db);            
               -- scan each and every dir entry in the sector
                  J := 0;
                  while J < 512 loop
                     Found := True;
                     if db (J) = 0 then
                     -- this entry is empty and it's the last one, so...
                        Found := False; 
                        exit Main_Loop;
                     end if;
                     if db (J) /= 16#E5# then
                     -- this entry contains valid data
                        if db (J) = 16#05# then
                        -- the actual db(J) should be 16#E5# so we change
                           db (J) := 16#E5#;
                        end if;
                        if (db (J + 11) / 16) rem 2 = 1 then
                        -- it's a directory
                           for Index in 1 .. 11 loop
                              if db (J + Index - 1) /= Byte_Name (Index)
                              then
                                 Found := False;
                                 exit;
                              end if;
                           end loop;
                        else
                        -- it's not a directory
                           Found := False;
                        end if;
                        exit Main_Loop when Found;
                     end if;
                     J := J + 32;
                  end loop;
                  I := I + 1;
               end loop;
               K := FAT_Cluster_Entry_Value (Partition_Nr, K);
            end loop Main_Loop;
         
            If Found then
               First_Cluster_of_Dir := To_Word (db, Word (J + 26));
            else 
               raise File_Not_Found_Exception;
            end if;
         
         end cd_relative;
      
         procedure cd_partition (New_Partition: in Natural) is
         begin 
            if Partition_Nr <= 4 then
               Partition_Nr         := New_Partition;
               First_Cluster_of_Dir := 0;
            else 
               raise Invalid_Partition_Exception; 
            end if;
         end cd_partition;
      
      end Path;
   
   ---[ File Handler ]---------------------------------------------------
      function Create_File_Name (File_Name: in String)
      return File is
      begin
         return To_Bounded_String (File_Name);
      end Create_File_Name;
   
      protected body File_Handler is
      
         function Is_Closed
         return Boolean is
         begin
            return Mode = Closed;
         end Is_Closed;
      
      
         entry Open (File_Name:    in File;
                     Path_Context: in Path      := Default_Path;
                     Open_Mode:    in File_Mode := Read_Only)
         when Is_Closed is
         
            db:         Data_Block;
            Found:      Boolean;
            I:          Byte;
            J:          Natural;
            K:          Word; 
            Byte_Name:  FAT_Name;
            Valid_Open: Boolean    := True;
         
         begin 
         
            Byte_Name := To_FAT_Name (File_Name);
            K := Path_Context.Get_First_Cluster_of_Dir;
         
         Main_Loop:
         -- scan each cluster of the directory
            while K < 16#FFF8# loop
            -- scan every sector of every cluster
               I := 0;
               while I < Partition (Partition_Nr).Sectors_per_Cluster
               loop
                  IDE_Driver.Dev_IO (Read,
                                     First_Sector_of_Cluster
                                        (Path_Context.Get_Partition_Nr,
                                         K) +
                                     DWord (I),
                                     db);
               -- scan each and every dir entry in the sector
                  J := 0;
                  while J < 512 loop
                     Found := True;
                     if db (J) = 0 then
                     -- this entry is empty and it's the last one, so...
                        Found := False; 
                        exit Main_Loop;
                     end if;
                     if db (J) /= 16#E5# then
                     -- this entry contains valid data
                        if db (J) = 16#05# then
                        -- the actual db(J) should be 16#E5# so we change
                           db (J) := 16#E5#;
                        end if;
                        if (db (J + 11) / 16) rem 2 = 0 then
                        -- it's a regular file
                           for Index in 1 .. 11 loop
                              if db (J + Index - 1) /= Byte_Name (Index)
                              then
                                 Found := False;
                                 exit;
                              end if;
                           end loop;
                        else
                        -- it's not a regular file
                           Found := False;
                        end if;
                        exit Main_Loop when Found;
                     end if;
                     J := J + 32;
                  end loop;
                  I := I + 1;
               end loop;
               K := FAT_Cluster_Entry_Value
                       (Path_Context.Get_Partition_Nr, K);
            end loop Main_Loop;
         
            If Found then
            -- set the required parameters
               Partition_Nr          := Path_Context.Get_Partition_Nr;
               First_Cluster_of_File := To_Word  (db, Word (J + 26));
               File_Size             := To_DWord (db, Word (J + 28));
               Mode                  := Open_Mode;
               Cursor_Position       := 0;
            else 
               raise File_Not_Found_Exception;
            end if;
         
         end Open;
      
         entry Read_Sector (Where:          out Data_Block;
                            EOF_Mark_At:    out Natural;
                            Sector:      in     DWord      := 0)
         when not Is_Closed is
         
         begin 
         
         -- check if the Sector is in range
            if Sector >= (File_Size + 511) / 512 then
               raise End_Of_File_Exception; 
            end if;
         
         -- we are in range, so we read the required sector
            IDE_Driver.Dev_IO (Read,
                               First_Sector_of_Cluster
                                  (Partition_Nr, First_Cluster_of_File) +
                               Sector,
                               Where);
         
         -- if the EOF is in this sector then we fetch its position into
         -- EOF_Mark_At, else we write 512 into EOF_Mark_At
            if Sector = (File_Size + 511 ) / 512 - 1 then
               EOF_Mark_At := Natural ((File_Size - 1) rem 512);
            else
               EOF_Mark_At := 512;
            end if;
         
         end Read_Sector;
      
         entry Close
         when not Is_Closed is
         begin
            Mode := Closed;
         end Close;
      
      end File_Handler;
   
   ---[ Interface ]------------------------------------------------------
      procedure Init is
      
         package Int_IO is new Integer_IO (Integer);
      
         db:                Data_Block;
         Data_Sectors:      DWord;
         Count_of_Clusters: DWord;
         FAT_Type:          Byte;
      
      begin 
      
         if Initialized then 
            return; 
         end if;
      
         IDE_Driver.Dev_IO (Read, 0, db);
      
         Put ("Valid partitions: ");
      
      -- locate the partitions in the hard disk
         for I in Partition_Table'Range loop
            Partition (I).First_Sector := To_DWord (db, P(I) +  8);
            Partition (I).Size         := To_DWord (db, P(I) + 12);
            if  db (Integer (P (I)) + 4) /= 4      -- DOS with FAT16
            and db (Integer (P (I)) + 4) /= 6 then -- Dos partition >32Mb
               Partition (I).Valid_FAT_Volume := False;
            else
               Partition (I).Valid_FAT_Volume := True;
            end if;
         end loop;
      
      -- setup partition parameters
         for I in Partition_Table'Range loop
            if Partition (I).Valid_FAT_Volume = True then
               IDE_Driver.Dev_IO (Read,
                                  Partition (I).First_Sector,
                                  db);
               Partition (I).Sectors_per_Cluster :=          db (13);
               Partition (I).Reserved_Sectors    := To_Word (db, 14);
               Partition (I).Num_FATs            :=          db (16);
               Partition (I).Root_Dir_Entries    := To_Word (db, 17);
               Partition (I).FAT_Size            := To_Word (db, 22);
               If Partition (I).FAT_Size = 0 then 
                  FAT_Type := 32; 
               end if;
            
               Partition (I).Root_Dir_Sectors :=
                     ((Partition (I).Root_Dir_Entries * 32) + 511) / 512;
            
               Partition (I).First_Root_Dir_Sector :=
                  DWord (Partition (I).Reserved_Sectors) +
                  DWord (Partition (I).Num_FATs) *
                  DWord (Partition (I).FAT_Size);
            
               Partition (I).First_Data_Sector :=
                  Partition (I).First_Root_Dir_Sector +
                  DWord (Partition (I).Root_Dir_Sectors);
            
            -- please, note that the next formula is correct, as we toke
            -- it from the Microsoft Hardware White Paper:
            -- FAT: General Overview of On-Disk Format
            -- version 1.02, May 5, 1999
               Data_Sectors := Partition (I).Size -
                               Partition (I).First_Data_Sector;
            
               Count_of_Clusters := Data_Sectors /
                                    DWord (Partition (I).
                                           Sectors_per_Cluster);
            
               if Count_of_Clusters < 4085 then
                  Fat_Type := 12;
               elsif Count_of_Clusters < 65525 then
                  Fat_Type := 16;
               else
                  Partition (I).Valid_FAT_Volume := False;
               end if;
            
               if Fat_Type = 16 then
                  Partition (I).Valid_FAT_Volume := True;
                  Put ("(hd0,");
                  Int_IO.Put (I, Width => 0);
                  Put (") ");
               else 
                  Partition (I).Valid_FAT_Volume := False;
               end if;
            
            end if;
         end loop;
      
         New_Line;
         Initialized := True;
         Default_Path.cd_partition (0);
      
      end Init;
   
   begin 
   
      Init;
   
   end fs.fs;

