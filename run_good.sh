for file in good/*.agl
do
    echo "Running $file..."
    ./interpreter $file
    echo
done