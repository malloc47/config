 {config, pkgs, lib, ...}:

 let
   cfg = config.settings;
 in

 with lib;

 {
   options = {
     settings = {
       username = mkOption {
         default = "malloc47";
         type = with types; uniq string;
       };
       email = mkOption {
         default = "malloc47@gmail.com";
         type = with types; uniq string;
       };
       vm = mkOption {
         type = types.bool;
         default = false;
       };
       terminal = mkOption {
         default = "alacritty";
         type = with types; uniq string;
       };
     };
   };
 }
