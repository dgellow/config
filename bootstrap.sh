for x in `find * -maxdepth 0 -name "$(basename "$0")" -prune -o -print`
do
  ln -s -i $PWD/$x $HOME/.$x
done
