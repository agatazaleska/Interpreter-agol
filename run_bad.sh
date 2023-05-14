for file in bad/*.agl
do
    echo "Running $file ..."
    ./interpreter $file
    echo
done