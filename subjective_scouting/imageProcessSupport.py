import zipfile
import os
import re
from io import BytesIO
import numpy as np
from PIL import Image
import csv
import struct



def load_images_from_zip(zip_path):
    blue_images = []
    red_images = []    
    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        for file_name in zip_ref.namelist():
            # Skip non-image files
            if not file_name.lower().endswith(('.png', '.jpg', '.jpeg')):
                continue

            # Extract base filename (in case of folders inside zip)
            base_name = os.path.basename(file_name)

            # Match pattern like B1.png or R23.png
            match = re.match(r'([BR])(\d+)\.(png|jpg|jpeg)', base_name, re.IGNORECASE)
            if match:
                color = match.group(1).upper()
                match_number = int(match.group(2))

                # Read image from zip without saving to disk
                with zip_ref.open(file_name) as image_file:
                    image_data = Image.open(BytesIO(image_file.read()))
                    image_data.load()  # Ensure image is fully loaded

                # Append as tuple (match_number, image)
                if color == 'B':
                    blue_images.append((match_number, image_data))
                elif color == 'R':
                    red_images.append((match_number, image_data))

    # Sort by match number
    blue_images.sort(key=lambda x: x[0])
    red_images.sort(key=lambda x: x[0])

    # Remove match numbers, keep only images
    blue_images = [img for _, img in blue_images]
    red_images = [img for _, img in red_images]
    
    for i in range(len(blue_images)):
        blue_images[i].save("pathImages//blue//" + f"B{i+1}.png")
    for i in range(len(red_images)):
        red_images[i].save("pathImages//red//" + f"R{i+1}.png")

def hex_to_rgb(hex):
    hex = hex[1:]
    return tuple(int(hex[i:i+2], 16) for i in (0, 2, 4))

def filter(match_num, team_color, image, color1, color2, color3):
    
    color1 = hex_to_rgb(color1)
    color2 = hex_to_rgb(color2)
    color3 = hex_to_rgb(color3)

    image = Image.open(image)
    image = image.convert("RGBA")
    img_array = np.array(image)
    
    out1 = Image.new('RGBA', image.size, color1 + tuple([0]))
    out2 = Image.new('RGBA', image.size, color2 + tuple([0]))
    out3 = Image.new('RGBA', image.size, color3 + tuple([0]))

    width, height = image.size
    
    for x in range(width):
        for y in range(height):
            (r,g,b,a) = image.getpixel((x,y))
            if (r,g,b) == color1:
                out1.putpixel((x,y), color1 + tuple([255]))
            elif (r,g,b) == color2:
                out2.putpixel((x,y), color2 + tuple([255]))
            elif (r,g,b) == color3:
                out3.putpixel((x,y), color3 + tuple([255]))
    
    out1.save("pathImages//matches//" + f"{round(match_num)}//{team_color}1.png")
    out2.save("pathImages//matches//" + f"{round(match_num)}//{team_color}2.png")
    out3.save("pathImages//matches//" + f"{round(match_num)}//{team_color}3.png")

def team_assign(image, team, color, number):
    image = Image.open(image)
    
    image.save("pathImages//teams//" + f"{round(team)}//{color}{round(number)}.png")

def fill(image, color):
    out = Image.open(image)
    
    color = hex_to_rgb(color)
    
    width, height = out.size
    
    for x in range(width):
        for y in range(height):
            (r,g,b,a) = out.getpixel((x,y))
            if(a != 0):
                out.putpixel((x,y), color + tuple([48]))
            else:
                out.putpixel((x,y), color + tuple([0]))
    
    out.save(image)

def clear(image):
    image = Image.open(image)
    out = Image.new('RGBA', image.size, (0,0,0,0))
    out.save("pathImages//empty.png")
    return out
