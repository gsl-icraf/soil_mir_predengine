import re

with open('www/css/custom.css', 'r') as f:
    css = f.read()

# 1. Replace the primary earth colors in :root
css = re.sub(r'--primary-color:\s*#7A5230;', '--primary-color: #00AAFF;', css)
css = re.sub(r'--secondary-color:\s*#8B6F47;', '--secondary-color: #00D4BB;', css)
css = re.sub(r'--success-color:\s*#3D7A45;', '--success-color: #00CC6A;', css)
css = re.sub(r'--warning-color:\s*#C17A3A;', '--warning-color: #FFCC00;', css)
css = re.sub(r'--danger-color:\s*#A0402A;', '--danger-color: #FF6600;', css)
css = re.sub(r'--info-color:\s*#5F7F8A;', '--info-color: #0055FF;', css)

# 2. Update --earth-wash to a cooler, lighter wash
css = re.sub(
    r'--earth-wash: linear-gradient\(135deg,\s*rgba\(139, 90, 43, 0\.03\) 0%,\s*rgba\(193, 122, 58, 0\.03\) 50%,\s*rgba\(122, 82, 48, 0\.03\) 100%\s*\);',
    '--earth-wash: linear-gradient(135deg, rgba(0, 170, 255, 0.03) 0%, rgba(0, 212, 187, 0.03) 50%, rgba(102, 0, 204, 0.03) 100%);',
    css, flags=re.DOTALL
)

# 3. Layout colors
css = re.sub(r'color: #1C1208;', 'color: #1E293B;', css)
css = re.sub(r'background: #F5F1EB;', 'background: #F8FAFC;', css)

# 4. Hero banner
css = re.sub(r'linear-gradient\(135deg, #1C1208 0%, #2A1E10 100%\)', 'linear-gradient(135deg, #F0F4F8 0%, #E2E8F0 100%)', css)
css = re.sub(
    r'rgba\( 92,  58,  30, 0\.55\).*?rgba\( 92,  58,  30, 0\.45\)',
    'rgba(0, 170, 255, 0.15) 0%, rgba(0, 212, 187, 0.1) 30%, rgba(255, 204, 0, 0.05) 60%, rgba(102, 0, 204, 0.1) 100%',
    css, flags=re.DOTALL
)

# 5. Banner title (make it a cool spectral gradient)
css = re.sub(
    r'background: linear-gradient\(135deg,\s*#F0DEC0 0%,\s*#D4A96A 35%,\s*#C17A3A 65%,\s*#8B5E2A 100%\s*\);',
    'background: linear-gradient(135deg, #0055FF 0%, #00AAFF 35%, #00D4BB 65%, #00CC6A 100%);',
    css, flags=re.DOTALL
)

# 6. Navbar updates (from dark brown to a lighter glassmorphic white/gray)
css = re.sub(
    r'background: linear-gradient\(135deg,\s*#1C1208 0%,\s*#2A1E10 40%,\s*#221A0C 70%,\s*#1A1006 100%\s*\) !important;',
    'background: rgba(255, 255, 255, 0.8) !important;',
    css, flags=re.DOTALL
)
css = re.sub(r'color: rgba\(240, 228, 210, 0\.95\) !important;', 'color: #0F172A !important;', css)
css = re.sub(r'color: #F0E4D0 !important;', 'color: #00AAFF !important;', css)
css = re.sub(r'color: rgba\(230, 215, 190, 0\.88\) !important;', 'color: #475569 !important;', css)

# 7. Button gradients
css = re.sub(r'linear-gradient\(135deg, #7A5230 0%, #C17A3A 100%\)', 'linear-gradient(135deg, #0055FF 0%, #00AAFF 100%)', css)

# 8. Card Header gradient
css = re.sub(
    r'linear-gradient\(135deg,\s*#2A1E10 0%,\s*#3A2818 50%,\s*#2A1E10 100%\s*\) !important;',
    'linear-gradient(135deg, #F8FAFC 0%, #F1F5F9 50%, #E2E8F0 100%) !important; color: #1E293B !important;',
    css, flags=re.DOTALL
)

# 9. .instrument-card to use glassmorphism
new_instrument_card = """
.instrument-card {
  border: 1px solid rgba(255, 255, 255, 0.4);
  border-top: 3px solid var(--primary-color);
  border-radius: 12px;
  box-shadow: 0 8px 32px rgba(31, 38, 135, 0.05);
  background: rgba(255, 255, 255, 0.65);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  position: relative;
  padding-top: 1.5rem;
}
"""
css = re.sub(r'\.instrument-card\s*\{[^}]+\}', new_instrument_card.strip(), css, flags=re.DOTALL)

# Update .instrument-label
new_instrument_label = """
.instrument-label {
  position: absolute;
  top: 0;
  left: 0;
  background: var(--primary-color);
  color: #fff;
  font-size: 0.65rem;
  padding: 3px 10px;
  text-transform: uppercase;
  font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
  border-bottom-right-radius: 8px;
  border-top-left-radius: 11px;
}
"""
css = re.sub(r'\.instrument-label\s*\{[^}]+\}', new_instrument_label.strip(), css, flags=re.DOTALL)

# 10. Replace all occurrences of rgba(122, 82, 48, ...) and rgba(193, 122, 58, ...) with cooler blues/grays
css = re.sub(r'rgba\(122, 82, 48, (0\.\d+)\)', r'rgba(0, 170, 255, \1)', css)
css = re.sub(r'rgba\(193, 122, 58, (0\.\d+)\)', r'rgba(0, 212, 187, \1)', css)
css = re.sub(r'rgba\(92, 58, 30, (0\.\d+)\)', r'rgba(102, 0, 204, \1)', css)
css = re.sub(r'rgba\(30, 16, 4, (0\.\d+)\)', r'rgba(15, 23, 42, \1)', css)

# 11. Lighten borders and fonts
css = re.sub(r'border: 1.5px solid rgba\(0, 170, 255, 0.22\);', 'border: 1px solid rgba(0, 170, 255, 0.2);', css)

# 12. Fix the bg-gradient class to be lighter and more glassy
css = re.sub(r'\.bg-gradient text-white', '.bg-gradient text-dark', css)

# 13. Update body and cards theme variables
css = re.sub(r'--bg: #F5F1EB;', '--bg: #F8FAFC;', css)
css = re.sub(r'--fg: #1C1208;', '--fg: #1E293B;', css)
css = re.sub(r'--card-bg: #FFFFFF;', '--card-bg: rgba(255, 255, 255, 0.65);', css)
css = re.sub(r'--accent: #C17A3A;', '--accent: #00AAFF;', css)

with open('www/css/custom.css', 'w') as f:
    f.write(css)

