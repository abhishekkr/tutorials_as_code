#!bash

############# base binaries
sudo rpm -ivh http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-stable.noarch.rpm
sudo dnf update --refresh

sudo dnf -y groupinstall "Development Tools"
sudo dnf -y install vim zsh wget curl openssl
sudo dnf -y install git clojure golang ghc gcc
sudo dnf -y install docker lxc
sudo dnf -y install mplayer gnome-mplayer vlc ffmpeg
sudo dnf -y install terminator patchelf thunar nemo firefox

sudo pip install --upgrade pip
sudo pip install virtualenv

virtualenv ~/.venv
source ~/.venv/bin/activate
pip install -U docker-compose
deactivate


############# docker stuff
sudo systemctl start docker
sudo systemctl enable docker
sudo chown $USER:$USER /var/run/docker.sock

############# google stuff
GOOGLE_REPO="/etc/yum.repos.d/google-chrome.repo"
echo "[google-chrome]" | sudo tee $GOOGLE_REPO
echo "name=google-chrome - \$basearch" | sudo tee -a $GOOGLE_REPO
echo "baseurl=http://dl.google.com/linux/chrome/rpm/stable/\$basearch" | sudo tee -a $GOOGLE_REPO
echo "enabled=1" | sudo tee -a $GOOGLE_REPO
echo "gpgcheck=1" | sudo tee -a $GOOGLE_REPO
echo "gpgkey=https://dl-ssl.google.com/linux/linux_signing_key.pub" | sudo tee -a $GOOGLE_REPO
unset GOOGLE_REPO
sudo dnf -y install google-chrome-stable

############# direct binaries
LOCALBIN="$HOME/ABK/bin"
USERPROFILE="$HOME/.bash_profile"

mkdir -p $LOCALBIN
cd $LOCALBIN
## CoreOS RKT
wget https://github.com/coreos/rkt/releases/download/v0.7.0/rkt-v0.7.0.tar.gz
tar xzvf rkt-v0.7.0.tar.gz
rm -f rkt-v0.7.0.tar.gz
cd rkt-v0.7.0
echo "export PATH=\$PATH:$PWD" >> $USERPROFILE
cd ..
. $USERPROFILE
rkt help

############# rbenv RUBY
git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> $USERPROFILE
echo 'eval "$(rbenv init -)"' >> $USERPROFILE
git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
. $USERPROFILE
type rbenv
rbenv install 2.2.0
rbenv shell 2.2.0

############# installing rust
curl -f -L https://static.rust-lang.org/rustup.sh -O
sh rustup.sh --disable-sudo --prefix=$LOCALBIN/rust
echo "export PATH=\$PATH:$LOCALBIN/rust/bin" >> $USERPROFILE
echo "export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:$LOCALBIN/rust/lib" >> $USERPROFILE
. $USERPROFILE
rustc --version



