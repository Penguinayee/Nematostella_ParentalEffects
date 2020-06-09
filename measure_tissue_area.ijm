run("Duplicate...", "duplicate channels=2"); //using F-actin channel for skeletonize the tissue
run("Gaussian Blur...", "sigma=8");
setThreshold(31, 255);
setOption("BlackBackground", true);
run("Create Selection");
run("Measure");
