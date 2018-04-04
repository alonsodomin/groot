def require_plugin(plugin_name)
  unless Vagrant.has_plugin?(plugin_name)
    puts "ERROR: Missing plugin '#{plugin_name}'"
    puts "Please run: vagrant plugin install #{plugin_name}"
    exit(1)
  end
end

Vagrant.configure("2") do |config|
  require_plugin("vagrant-hostmanager")
  require_plugin("vagrant-vbguest")

  config.vm.box = "centos/7"

  local_cabal_cache = File.expand_path("~/.cabal")
  if File.exist?(local_cabal_cache)
    config.vm.synced_folder local_cabal_cache, "/home/vagrant/.cabal",
                            id: "cabal-cache",
                            mount_options: ["dmode=777,fmode=777"]
  end

  config.vm.provision "shell", path: "scripts/vagrant_setup.sh",
                               privileged: false
end