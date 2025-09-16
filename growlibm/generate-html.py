import json
import os

# --- 1. Configuration ---
START_REPORT_PATH = 'reports/start/results.json'
END_REPORT_PATH = 'reports/end/results.json'
START_FOLDER = 'start'
END_FOLDER = 'end'
OUTPUT_FILE = 'reports/index.html'

# Set the number of iteration links you want to generate
NUM_ITERATIONS = 10 

# --- 2. Calculation Function ---
def calculate_average_percentage_accurate(data):
    """
    Calculates average percentage accurate for both start and end states.
    Matches the logic from the provided JavaScript.
    """
    total_start = 0
    total_end = 0
    max_accuracy = 0  # This is the sum of all 'bits'

    for test in data.get("tests", []):
        total_start += test.get("start", 0)
        total_end += test.get("end", 0)
        max_accuracy += test.get("bits", 0)

    if max_accuracy == 0:
        return 0.0, 0.0

    # Calculate the percentage for the start and end values
    start_decimal = total_start / max_accuracy
    end_decimal = total_end / max_accuracy

    start_percent = 100 - (100 * start_decimal)
    end_percent = 100 - (100 * end_decimal)

    return start_percent, end_percent

# --- 3. Load Data and Calculate Percentages ---
try:
    with open(START_REPORT_PATH, 'r') as f:
        start_data = json.load(f)
    with open(END_REPORT_PATH, 'r') as f:
        end_data = json.load(f)
except FileNotFoundError as e:
    print(f"Error: Could not find a report file: {e.filename}")
    exit()

start_report_start_pct, start_report_end_pct = calculate_average_percentage_accurate(start_data)
end_report_start_pct, end_report_end_pct = calculate_average_percentage_accurate(end_data)

# --- 4. Generate Links for Iteration Folders ---
table_rows_html = ""
# The loop now uses the magic constant
for i in range(NUM_ITERATIONS):
    folder_name = f"iter{i}"
    table_rows_html += f"""
    <tr>
        <td><a href="{folder_name}/">{folder_name}</a></td>
    </tr>"""

# --- 5. Create the Full HTML Page ---
html_template = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Growlibm</title>
    <style>
        body {{ font-family: sans-serif; margin: 40px; color: #333; }}
        .container {{ max-width: 900px; margin: auto; }}
        .summary {{ background-color: #f4f4f9; border-left: 5px solid #6c757d; padding: 15px 20px; margin-bottom: 30px; }}
        h1, h2 {{ color: #0056b3; }}
        table {{ border-collapse: collapse; width: 100%; }}
        th, td {{ border: 1px solid #ccc; text-align: left; padding: 12px; }}
        th {{ background-color: #e9ecef; }}
        a {{ color: #007BFF; text-decoration: none; font-weight: bold; }}
        a:hover {{ text-decoration: underline; }}
    </style>
</head>
<body>
    <div class="container">
        <h1>GrowLibm Report</h1>
        <p><a href="grow_platform.txt">Grow Platform</a></p>

        <div class="summary">
            <h2>Overall Summary</h2>
            <p>
                <strong>Start:</strong> 
                {start_report_start_pct:.1f}% → {start_report_end_pct:.1f}%
                - <a href="{START_FOLDER}/">Folder</a>
            </p>
            <p>
                <strong>End:</strong> 
                {end_report_start_pct:.1f}% → {end_report_end_pct:.1f}%
                - <a href="{END_FOLDER}/">Folder</a>
            </p>
        </div>
        
        <h2>Grow Platform Iterations</h2>
        <table>
            <thead>
                <tr>
                    <th>Link to Iteration</th>
                </tr>
            </thead>
            <tbody>
                {table_rows_html}
            </tbody>
        </table>
    </div>
</body>
</html>
"""

# --- 6. Write the HTML to a File ---
with open(OUTPUT_FILE, 'w') as f:
    f.write(html_template)

print(f"Successfully generated index.html")