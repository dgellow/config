for x in `ls --ignore="$(basename "$0")"`
do
  ln -s -i $PWD/$x $HOME/.$x
done
