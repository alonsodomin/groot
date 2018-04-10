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

  config.vm.define "centos" do |centos|
    config.vm.box = "centos/7"
  end
  config.vm.define "ubuntu" do |ubuntu|
    config.vm.box = "ubuntu/trusty64"
  end

  # config.vm.provision "shell", path: "scripts/centos_setup.sh",
  #                              privileged: false
end