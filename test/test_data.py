#!/usr/bin/env python3

import numpy as np

# Set up x values
#x_values = np.linspace(0, 2.*np.pi, num=10000)
x_values = np.linspace(0., 4., num=10000)

# Calculate square(x) for each x value
# sin_values = 0.5 * (np.sin(x_values) + 1.0)
square_values = x_values * x_values

# Write x and sin(x) values to a file
with open("square_values.txt", "w") as f:
    for x, square_x in zip(x_values, square_values):
        f.write(f"{x} {square_x}\n")
