{ pkgs, inputs, ... }: {

    nixpkgs.overlays = [
      (_: _: {
        redirect-to-xpsoasis-wrapper = inputs.self.packages.x86_64-linux.redirect-to-xpsoasis-wrapper;
      })
    ];

    networking.firewall.allowedTCPPorts = [ 22 80 5432 587 443 3000 ];

    systemd.services.redirect-to-xpsoasis = {
      description = "redirect-to-xpsoasis";
      enable = true;
      wantedBy = [ "multi-user.target" "nginx.service" ];
      after = [ "network.service" "local-fs.target" ];
      environment = {
        YESOD_STATIC_DIR="/home/xpsoasis/redirect-to-xpsoasis/static";
        YESOD_PORT="3000";
        YESOD_APPROOT="https://rdataa.com";
      };
      serviceConfig = {
        Type = "simple";
        User = "admin";
        WorkingDirectory = "/home/xpsoasis/redirect-to-xpsoasis";
        ExecStart = ''${pkgs.redirect-to-xpsoasis-wrapper}/bin/redirect-to-xpsoasis-wrapped'';
        ExecStop = "";
        Restart = "always";
      };
    };

    # security.acme.acceptTerms = true;
    # security.acme.defaults.email = "enriquead@rdataa.com";

    services.nginx = {
      enable = true;

      virtualHosts."rdataa.com" = {
        # enableACME = true;
        # forceSSL = true;
        locations = {
          "/" = {
            proxyPass = "http://localhost:3000";
          };
        };
      };
    };

    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_11;
      enableTCPIP = true;
      authentication = pkgs.lib.mkOverride 10 ''
        local all all trust
        host all all ::1/128 trust
      '';
      initialScript = pkgs.writeText "backend-initScript" ''
        CREATE ROLE redirect WITH LOGIN PASSWORD 'xpsoasis';
        CREATE DATABASE redirect;
        GRANT ALL PRIVILEGES ON DATABASE redirect TO redirect;
      '';
    };

    environment.systemPackages = with pkgs; [
      openssh
      git
      msmtp
      vim
      emacs
      zsh
    ];
}
