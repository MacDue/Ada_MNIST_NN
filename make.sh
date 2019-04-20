gnatmake -O3 -aI/home/macdue/Downloads/png_5_0 -aI/home/macdue/Downloads/zlibada -aO/home/macdue/Downloads/png_5_0 -aO/home/macdue/Downloads/zlibada mnist_train_main -largs -L/usr/lib/x86_64-linux-gnu/libz.a -lz


gnatmake -O3 -aI/home/macdue/Downloads/png_5_0 -aI/home/macdue/Downloads/zlibada -aO/home/macdue/Downloads/png_5_0 -aO/home/macdue/Downloads/zlibada mnist_test_main -largs -L/usr/lib/x86_64-linux-gnu/libz.a -lz


ADA_PROJECT_PATH="/usr/lib/sdlada/share/gpr/" gnatmake -p -P visualise.gpr   
