{ config, pkgs, ... }:
{
  imports = [ ../modules/settings.nix ];

  home.packages = [pkgs.choose-gui pkgs.clipcat];

  xdg.configFile."clipcat/clipcatd.toml".text = ''
    daemonize = true
    pid_file = "/run/user/1000/clipcatd.pid"
    primary_threshold_ms = 5000
    max_history = 50
    synchronize_selection_with_clipboard = true
    history_file_path = "/home/jwaggoner/.cache/clipcat/clipcatd-history"
    snippets = []

    [log]
    emit_journald = true
    emit_stdout = false
    emit_stderr = false
    level = "INFO"

    [watcher]
    enable_clipboard = true
    enable_primary = true
    enable_secondary = false
    sensitive_x11_atoms = ["x-kde-passwordManagerHint"]
    filter_text_min_length = 1
    filter_text_max_length = 20000000
    denied_text_regex_patterns = []
    capture_image = true
    filter_image_max_size = 5242880

    [grpc]
    enable_http = true
    enable_local_socket = true
    host = "127.0.0.1"
    port = 45045
    local_socket = "/run/user/1000/clipcat/grpc.sock"

    [dbus]
    enable = true

    [metrics]
    enable = true
    host = "127.0.0.1"
    port = 45047

    [desktop_notification]
    enable = true
    icon = "accessories-clipboard"
    timeout_ms = 2000
    long_plaintext_length = 2000
  '';

  xdg.configFile."clipcat/clipcatctl.toml".text = ''
    server_endpoint = "/run/user/1000/clipcat/grpc.sock"
    preview_length = 100

    [log]
    emit_journald = true
    emit_stdout = false
    emit_stderr = false
    level = "INFO"
  '';

  xdg.configFile."clipcat/clipcat-menu.toml".text = ''
    server_endpoint = "/run/user/1000/clipcat/grpc.sock"
    finder = "rofi"
    preview_length = 80

    [rofi]
    line_length = 100
    menu_length = 30
    menu_prompt = "Clipboard"
    extra_arguments = []

    [dmenu]
    line_length = 100
    menu_length = 30
    menu_prompt = "Clipboard"
    extra_arguments = []

    [choose]
    line_length = 100
    menu_length = 30
    menu_prompt = "Clipboard"
    extra_arguments = []

    [custom_finder]
    program = "fzf"
    args = []

    [log]
    emit_journald = true
    emit_stdout = false
    emit_stderr = false
    level = "INFO"
  '';
}
