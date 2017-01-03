--  Dummy package intended to represent the standard System package
--  This makes it easier to make it appear that a child of System is being
--  used, since you cant declare your own child packages
--  under System. In the  Ada 2012 version, this package no longer exists, as
--  System has all the needed child packages.
package Sys is
   pragma Pure;
end Sys;
