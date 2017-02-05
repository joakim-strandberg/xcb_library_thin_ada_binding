--  Dummy package intended to represent the standard System.Storage_Pools
--  package.
--  This makes it easier to make it appear that a child of System.Storage_Pools
--  is being used, since you cant declare your own child packages
--  under System. In the Ada 2012 version, this package no longer exists, as
--  System.Storage_Pools has all the needed child packages.
package Sys.Storage_Pools is
   pragma Preelaborate;
end Sys.Storage_Pools;
