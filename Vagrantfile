Vagrant.configure("2") do |config|
  config.vm.box = "centos/7"

  config.vm.provision "shell", path: "scripts/vagrant_setup.sh",
                               privileged: false
end