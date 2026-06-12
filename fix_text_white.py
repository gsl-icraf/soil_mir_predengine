import glob
import re

for filepath in glob.glob('modules/*.R'):
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Replace bg-gradient text-white with bg-gradient text-dark
    content = content.replace('bg-gradient text-white', 'bg-gradient text-dark')
    # Replace bg-primary text-white with bg-primary text-dark
    content = content.replace('bg-primary text-white', 'bg-primary text-dark')
    
    with open(filepath, 'w') as f:
        f.write(content)
