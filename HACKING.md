Contributing to HaLVM
====

Development Environment Setup
----

**NOTE**: it is not trivial to set up everything, esp. for the first-time contributor. And this guide is not exhaustively tested in different environments and not necessarily up-to-date. But that's is why HaLVM NEEDS YOUR HELP :) Please report any trouble you met that is not mentioned in this guide, or make a pull request fixing the problem. Any form of contribution is appreciated!

## Step #1: Set up the Host Virtual Machine

Why we need another VM? Because we need to install a Xen hypervisor on the host system, which is probably not something you want to do with your own machine. Besides, we can standardize the steps if we can assume the host OS. Also, it is easier to backup (You can **snapshot** at any check point as you like, and we recommend you to do so frequently before accidentally screwing things up).

Our Host OS of choice is **Fedora Server 22/23/24**. (Why "Server"? Because "Workstation" might have `xen-libs` etc. pre-installed, which might cause trouble for us. And "Server" is also more lightweight than "WorkStation").

You can download its image from any mirror site, for example:

- For Server 22: https://download.fedoraproject.org/pub/fedora/linux/releases/22/Server/x86_64/iso/Fedora-Server-DVD-x86_64-22.iso
- For Server 23: https://download.fedoraproject.org/pub/fedora/linux/releases/23/Server/x86_64/iso/Fedora-Server-DVD-x86_64-23.iso

You can also try 21, 25 etc. or even other distros, good luck and if any, please tell us any problem you met, or make a PR documenting how to solve it.

Next, please refer to different subsections depending on your VM softwares, though the steps are not much different.

### Option 1: VMWare

VMWare Fusion really wants you to install from a disk / image, or import an existing PC. Let's not do that. Instead, select "More options ...". At the next screen, we'll "Create a custom virtual machine," and then hit the continue button. When it asks us to select the operating system select **Other** and then **Other 64-bit**. This is _**very important**_. It may be very tempting to know that we're going to install Fedora at some point, and instead pick that specific option from the Linux menu. _Do not be tempted!_ The truth is that the OS we are going to install is Xen, not Linux, and the whole VM-within-a-VM stack tends to fall apart unless you choose `Other 64-bit`.

_**Important warning:**_ do **not** install the VMware Tools or Guest Additions at any point on your new machine. This will, like failing to choose `Other 64-bit`, cause the VM-within-a-VM stack to fall apart.

Now we get to create a virtual disk. I'd create a new one, unless you have one you want to destroy lying around. Don't worry about the capacity; we'll adjust that later.

VMWare will then give you a summary of your new machine. Click Finish, and then give it a name like "HaLVM".

When you hit save, it's going to start the VM, which isn't going to do anything. Shut it down, using the "Shut Down" item in the "Virtual Machine" menu.

OK, now let's go through and tweak a bunch of settings to make everything work better. In the Virtual Machine library, select your new Virtual Machine, and then click then click Settings (either in the right click menu, the toolbar, or from the "Virtual Machine" menu).

First thing, VMWare seems to live in the late 90s / early 2000s, and defaults to 256MB of memory for your machine. Let's bring things into the modern era, and bump that up via the "Processors & Memory" item. This will also improve your compile times. I set my VMs to use 4GB / 4096 MB, but you may give it more or less depending on how much memory your machine has available to it. However, I would strongly advise against using any less than 1GB / 1024MB.

When you're done, hit "Show All" at the top to return. Next step, network adapters.

The first thing to say about network adapters is that this is where you'll likely find the most variance from machine to machine, depending on your particular network setup. The following is one possible option. You may find, however, that other options work better for you.

That being said, we prefer to use two network adapters: one bridged to the local network, and the other private to the local machine. So select "Network Adapter" from the general settings window, and then select "Autodetect" under "Bridged Networking". This will automatically have the first network card bridge to your local network. Next, click the "Add Device" button in the top right corner. Select "Network Adapter", "Add", and then set this one to "Private to my Mac". Then "Show All" to get back to the main menu.

Again, this dual-NIC configuration isn't necessary, but we find it very convenient for debugging some network problems.

We're almost done now. Next step, click on "Hard Disk (IDE)". Give it more disk space. I tend to default all new VMs to 40GB as a nice base. Do not select less than 20GB, but feel free to pick your own amount based on your own available disk space. Click "Apply."

"Show All" gets you back to the main menu once again.

Now, set the previously downloaded Fedora up as a CD: Click "CD/DVD (IDE)", pick "Choose a disk or disk image", and select your newly-downloaded image. Then check the "Connect CD/DVD Drive" checkbox, and you're done with the Settings. Close the window, and be glad.

### Option 2: Virtual Box
The process is similar to the VMWare's above. Just to note:

1. Choose `Other 64-bit` rather than `Fedora`
2. Give it enough resource (>=2GB memory, >=30GB storage)

## Step #2: Install the Fedora and Connect it with `ssh`

During installation, pretty much everything is standard you can just use the default option. There are some more tips though:

1. For storage, there will be a warning icon over "Installation Destination" at first. Just click into it, and use the default "automatically configure partitioning." and confirm by "Done". Now it will go away.
2. If you have to choose packages to install, select "Minimal Installation" and confirm.
3. Remember to check the "Make this user administrator" when creating the account

Now, reboot the machine to finish installation. (For VirtualBox user: You might have to manually remove the installer disk image in `Settings -> Storage`).

After we restarted the Fedora, let's set up the `ssh` connection it from host machine. Why bother? first, we don't really need GUI and second, typing commands in a virtualized monitor is a pain.

After booting up, you will probably need to launch the ssh daemon:

```
$ sudo service sshd start
$ sudo chkconfig sshd on
```

### VMWare or VirtualBox: Method 1

At the console, run `ifconfig` and look for the IP address in the `inet` field of the result:

```
$ sudo dnf install net-tools # only necessary if ifconfig is not installed
$ ifconfig
enp0s3: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 192.168.XXX.YYY  netmask 255.255.254.0  broadcast 192.168.41.255
...
```

Then, on your host machine in a nicer terminal emulator, connect to that IP address with your username.

```
$ ssh <user-name>@192.168.XXX.YYY
```

### Tested with VirtualBox: Method 2

Method 1 is good, except that the IP address might not be fixed, which means that you have to peek into the VM window everytime you rebooted the Fedora. A better approach, at least working in VirtualBox, is **port-forwarding** (NOTE: I believe the VMWare also has such feature, please add something here if you know about it).

NOTE: NIC must be in NAT mode.

Follow "Settings -> Network -> Adaptor 1 -> Advanced -> Port Forwarding", then in the config dialog, we will add a new rule, with "Name" as "ssh" or something similarly meaningful, "Host Port" as "3333" or something similarly nonsensical, and "Guest Port" as "22", the rest kept as default.

Then, for *nix machines, you can add a ssh configuration by opening `~/.ssh/config`, and adding something like this:

```
Host halvm
     HostName 127.0.0.1
     Port 3333
     User halvm
```

Then here you go: use `ssh-copy-id <host-name>` to set up the key authentication (if you want to manually do this instead of using `ssh-copy-id`, https://www.digitalocean.com/community/tutorials/how-to-configure-ssh-key-based-authentication-on-a-linux-server might be helpful to you). Then `ssh <host-name>`, where `<host-name>` is the one after `Host` in the `~/.ssh/config`.

## Step #3: Install Xen and other build dependencies

Next, let's get Xen and other necessary tools installed.

First, let's tell our Fedora system about the HaLVM public repos. Be sure to replace `2X` in the commands below with the appropriate Fedora version number:

```
$ sudo dnf install http://repos.halvm.org/fedora-2X/x86_64/halvm-yum-repo-2X-3.fc2X.noarch.rpm
$ sudo dnf update
```

Now let's install the dependencies.

```
$ sudo dnf install gcc automake libtool patch ncurses-devel halvm-xen halvm-xen-devel zlib-devel git
$ sudo dnf install libgmp # for Fedora 22
$ sudo dnf install gmp-devel # for Fedora 23/24
$ sudo ln -s /usr/lib64/libtinfo.so.6 /usr/lib64/libtinfo.so.5 # for Fedora 24. It is a HACK
```

NOTE: Why a modified version of Xen? Debugging. The stock version of Xen that comes with Fedora has certain low-level debugging capabilities disabled, as they add some bulk and slowdown to the whole system. The `halvm-xen` version of Xen has been modified to enable those features, which allows for us to use (with some boot flags) a very reliable output mechanism for debugging. Our modified versions simply add the flag "verbose=y" to their build specification, on the lines that build and install the hypervisor (search for "dist-xen" and "install-xen"). We try to keep the latest version we're using available, in `src/misc/xen.spec`. In addition, I will try to keep recent binary and source RPMs available at `http://repos.halvm.org`.

Before we reboot -- yes, we're about to reboot again -- let's set up that boot flag I just mentioned. Open up `/etc/default/grub` with your favorite editor, with `sudo`, and add the following line:

```
GRUB_CMDLINE_XEN="console_to_ring loglvl=all guest_loglvl=all"
```

While you're at it, find the line that starts with `GRUB_DEFAULT`, and change it to:

```
GRUB_DEFAULT='Fedora, with Xen hypervisor'
```

Then update the boot loader:

```
$ sudo grub2-mkconfig -o /boot/grub2/grub.cfg
```

... and now we reboot! Let's validate if it works well.

Open a terminal. First, try this command:

```
$ sudo xl list
```

This gives you a list of all the Xen virtual machines running in your virtual machine. It should just list Dom0, and look something like this:

```
Name                               ID    Mem VCPUs    State   Time(s)
Domain-0                            0   3918     1   r----       42.7
```

If you get an error, then something went wrong. Maybe go back and see if you missed a step somewhere?

If that works, that means Xen is installed. Let's just triple-check the debugging options worked. Type the following command:

```
$ sudo xl dmesg
```

If the very end of this log looks like a Fedora console login prompt, then everything went fine.

## Step #4 (Optional): Setting Up The Network

To make certain HaLVM applications that depend on networking work, we need to configure our network. More concretely, this is because Xen expects to attach new, virtual network devices to a Dom0 bridge called `xenbr0`.

Briefly, what we're going to do is create a new bridge called `xenbr0`, make sure that the STP protocol is turned off, make sure that your Internet-facing Ethernet card is attached to it, and then make sure that all of this happens on every boot.

So, to start, make sure the network service is on and will stay on through reboots:

```
$ sudo chkconfig network on
```

Now create the configuration file for the bridge:

```
$ sudo vim /etc/sysconfig/network-scripts/ifcfg-xenbr0
```

Use your editor to add the following contents to the configuration:

```
DEVICE=xenbr0
TYPE=Bridge
BOOTPROTO=dhcp
ONBOOT=yes
DELAY=0
NM_CONTROLLED=no
```

Next, we want to add a network card to our bridge. First, we need to find the name of the interface:

```
$ sudo dnf install net-tools # if `ifconfig` is not installed
$ ifconfig
```

Look for the interface with the MAC address that matches your externally-visible network interface (you can see this in the advanced settings of the VM). In VMWare box, this might look like `eno16777728`; In VirtualBox, it might look like `enp0s3`. We will call it `enXXXX` in the following text. Edit the configuration file for that interface...

```
$ sudo vim /etc/sysconf/network-scripts/ifcfg-enXXXX
```

... and add the following lines:

```
BRIDGE=xenbr0
NM_CONTROLLED=no
```

Now restart the network service, and ping a friendly outside address to make sure you still have access to the wider Internet:

```
$ sudo service network restart
$ ping galois.com
```

If you've done all this, you've made it through a major hurdle in setting up these machines. Pat yourself on the back.

## Step #5: Build HaLVM!

If GHC 7.8.4 is not present, a version of it will be downloaded for
use during the build process.

Once checked out, the HaLVM builds as follows:

> git clone https://github.com/GaloisInc/HaLVM

> git submodule update --init --recursive

> autoconf

> ./configure

> make

> make install

> halvm-ghc-pkg recache

The configure system will accept and honor the "--prefix" flag as per
normal. We also strongly suggest using the "--enable-gmp" flag, in order
to enable the (much faster) GMP library for large integer math.

If you intend on using inter-domain communication (specified in
`Communication.Rendezvous` and `Communication.IVC`), run the `mkrenddir` script
(located wherever you installed the HaLVM, `/usr/local/bin` by default) as root. 
This creates a top-level `/rendezvous` directory in the XenStore with
appropriate permissions.

## Step #6: Test with some examples

There are many examples under `HaLVM/examples`, you can choose one, for example, `Core/Hello`, enter the folder, and `make; make run`. Note that for examples that use IVC, you have to run `sudo src/mkrenddir/mkrenddir` (under `HaLVM`) to set up the XenStore.

## Wrap-up

**Acknowledgements**: This guide is based on Adam Wick, Tim Humphries, Michael Hueschen
, Mark Wotton, Adam C. Foltzer, and Arthur Clune's previous efforts in writing something up. Since there are some other details that might be useful to you, here are links to them.

- https://github.com/GaloisInc/HaLVM/wiki
- https://github.com/GaloisInc/HaLVM/wiki/Building-a-Development-Virtual-Machine
- https://github.com/GaloisInc/HaLVM/blob/master/README.md

And also thanks to Arnaud Bailly for testing the instructions here and giving feedbacks.

Be a contributor
----

If you plan to do development work on the HaLVM itself, please fork the HaLVM. This allows us to more easily tell who is working on the HaLVM, and GitHub's tools make merging your changes much more easy.

TODO: More instructions.
