{ config, pkgs, ... }:

{
  users.users.admin = {
    name = "admin";
    createHome = true;
    isNormalUser = true;
    home = "/home/xpsoasis";
    description = "rdataa admin";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    initialPassword = "1234";
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDEFWtCBEDhaCfihSg2XiZ+yMTzrWquWG1Tgix+Snrw5Rz8AEaDigy4m72BsT/jMEigEi5Y1u6GznRvn7MMyRFAPZmVvyWUcXpopo5LJ2aryCKeq1DD/aSyEKbrShmgWaIF8nhDQ4VdbNzaXk01Q3iipN76vew7Bze/RtKCGh9+hGfJGIl47qsG6vjnz9V8SWru0CVmco2RPVrEoOs+1AIr2HnoZ9MSZi5hFVnrM26FGxHBOcZIhDWVbq3liDZrALLx6QIr8TPVpKqbXycRPHTZXQWClSH0X7AhYKpxA9KX3JeGKjrJMkG1FiBP/+umgUEgBEtmQUqWooH32t8jmO0jfAWGv559WYA6kndUI4nbJ8dmXkBw6NuhxBHv8uqxA59y7amsDDJHjxFADx6TF2+8h07f+Tjl6k2S45hv/t5oOMiR+cZ3Bj571XMXvcmTakOnRnVnSnXs1QBfn9tKK1ozuswCkUkDEtA9VHM7dm7EcRFdbRMDnFWomXbMmCtwO0U= omoper@roronoa" ];
  };
  security.sudo.wheelNeedsPassword = false;
  nix.settings.trusted-users = [ "@wheel" ]; # https://github.com/serokell/deploy-rs/issues/25
}
