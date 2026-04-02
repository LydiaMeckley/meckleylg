install.packages("reticulate")
library(reticulate)

# Create a dedicated python environment for YOLO
virtualenv_create("yolo_env")
virtualenv_install("yolo_env", c("ultralytics", "numpy", "opencv-python"))
use_virtualenv("yolo_env", required = TRUE)

# Import the YOLO class from the ultralytics python module
yolo <- import("ultralytics")$YOLO

# 1. Load a pre-trained model (e.g., YOLOv8 nano)
# It will automatically download the .pt file if not present
model <- yolo("yolov8n.pt")

# 2. Run identification on an image
results <- model$predict(source = "path/to/your/image.jpg", conf = 0.25)

# 3. View the results
# In R, results is a list of Python objects. 
# We can use 'plot' to see the annotated image (requires a GUI or saving to file)
res <- results[[1]]
res$show() # This opens a window with the detected objects
res$save(filename = "result.jpg") # Saves the annotated image

# Extract bounding box data
boxes <- res$boxes

# Convert to a data frame
detection_data <- data.frame(
  class_id = as.vector(boxes$cls$cpu()$numpy()),
  conf     = as.vector(boxes$conf$cpu()$numpy()),
  xmin     = as.vector(boxes$xyxy$cpu()$numpy()[,1]),
  ymin     = as.vector(boxes$xyxy$cpu()$numpy()[,2]),
  xmax     = as.vector(boxes$xyxy$cpu()$numpy()[,3]),
  ymax     = as.vector(boxes$xyxy$cpu()$numpy()[,4])
)

# Map class IDs to names
class_names <- res$names
detection_data$label <- sapply(detection_data$class_id, function(id) class_names[[as.character(id)]])

print(head(detection_data))