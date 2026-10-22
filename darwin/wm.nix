{
  config,
  pkgs,
  ...
}:
with pkgs.lib;
{
  imports = [
    ../modules/settings.nix
    ./aerospace.nix
    ./omniwm.nix
  ];

  home.packages = with pkgs; [
    # albert
    flameshot
  ];

  home.file."Library/Preferences/albert/config" = {
    target = "Library/Preferences/albert/config";
    source = ./. + "/../home/albert.conf";
  };

  launchd.agents.albert = {
    enable = true;
    config = {
      ProgramArguments = [
        # "${pkgs.albert}/Applications/Albert.app/Contents/MacOS/Albert"
        "/Applications/Albert.app/Contents/MacOS/Albert"
      ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  launchd.agents.flameshot = {
    enable = true;
    config = {
      ProgramArguments = [ "${pkgs.flameshot}/bin/flameshot" ];
      ProcessType = "Interactive";
      KeepAlive = {
        SuccessfulExit = true;
      };
      RunAtLoad = true;
    };
  };

  xdg.configFile."flameshot/flameshot.ini".text = ''
    [General]
    disabledTrayIcon=true
    useJpgForClipboard=true
    savePath=${config.home.homeDirectory}/Downloads

    [Shortcuts]
    SCREENSHOT_HISTORY=
    TAKE_SCREENSHOT=
  '';

  xdg.configFile."karabiner/karabiner.json".source = ./karabiner.json;

  ## This is not yet available in 25.05
  #programs.sketchybar = {
  #  enable = true;
  #};

}
