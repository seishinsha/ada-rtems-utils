-------------------------------------------------------------------------
--                                                                     --
--                     RTEMS FILE-SYSTEM SUPPORT                       --
--                                                                     --
--                            F S . I D E                              --
--                                                                     --
--                     S p e c i f i c a t i o n                       --
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


   package fs.ide is
   
      protected IDE_Driver is
      
         entry Init;
         entry Dev_IO (rw_flag: in     Read_or_Write;
                       Pos:     in     DWord;
                       Data:       out Data_Block);
      
      private
      
         Initialized: Boolean := False;
      
      end IDE_Driver;
   
      Disk_Error: exception;
   
   end fs.ide;

