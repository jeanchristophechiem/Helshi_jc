import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# Create a normal distribution with mean = 10 and sd = 5
np.random.seed(42)  # For reproducibility
y = np.random.normal(loc=10, scale=5, size=1000)

# Create histogram with density overlay
plt.figure(figsize=(10, 6))
plt.hist(y, bins=30, density=True, alpha=0.7, color='lightblue', 
         edgecolor='white', label='Histogram')

# Add density curve (kernel density estimation)
x_range = np.linspace(y.min(), y.max(), 100)
kde = stats.gaussian_kde(y)
density_curve = kde(x_range)
plt.plot(x_range, density_curve, color='red', linewidth=2, label='Density')

# Set labels and title
plt.xlabel('Values')
plt.ylabel('Density')
plt.title('Histogram with Density Overlay')

# Add legend
plt.legend()

# Add grid for better readability
plt.grid(True, alpha=0.3)

# Save the plot
plt.tight_layout()
plt.savefig('histogram_plot.png', dpi=300, bbox_inches='tight')
print("Plot saved as 'histogram_plot.png'")
plt.show()