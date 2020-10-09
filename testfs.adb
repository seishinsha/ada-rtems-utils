   with fs.fs;
   with Ada.Text_IO;

   procedure testfs is
   
      myPath: fs.fs.Path;
      myFile: fs.fs.File_Handler;
      Data:   fs.Data_Block;
      At_EOF: Natural;
      Final:  Natural;
      S:      fs.DWord;
   
   begin
   
      myPath.cd_partition (0);
      myPath.cd_absolute  (fs.fs.Create_File_Name ("alex"));
      myPath.cd_relative  (fs.fs.Create_File_Name ("config"));
      myFile.Open (fs.fs.Create_File_Name ("partida.cfg"), myPath);
   
      S := 0;
      loop
         myFile.Read_Sector (Data, At_EOF, S);
         if At_EOF = 512 then 
            Final := 511; 
         else 
            Final := At_EOF; 
         end if;
         for I in 0 .. Final loop
            Ada.Text_IO.Put (Character'Val (Data (I)));
         end loop;
         exit when Final = At_EOF;
         S := fs."+" (S, 1);
      end loop;
      myFile.Close;
      Ada.Text_IO.New_Line;
   
   end testfs;

