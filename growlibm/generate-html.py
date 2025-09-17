import json

# --- 1. Configuration ---
START_REPORT_PATH = 'reports/start/results.json'
END_REPORT_PATH = 'reports/end/results.json'
START_FOLDER = 'start'
END_FOLDER = 'end'
OUTPUT_FILE = 'reports/index.html'
NUM_ITERATIONS = 10 

def calculate_end_accuracy(data):
    total_end = 0
    max_accuracy = 0
    for test in data.get("tests", []):
        total_end += test.get("end", 0)
        max_accuracy += test.get("bits", 0)
    if max_accuracy == 0:
        return 0.0
    return 100 - (100 * (total_end / max_accuracy))

def get_accuracy_str(path):
    try:
        with open(path, 'r') as f:
            data = json.load(f)
        percent = calculate_end_accuracy(data)
        return f"{percent:.1f}%"
    except (FileNotFoundError, json.JSONDecodeError):
        return "N/A"

start_accuracy_str = get_accuracy_str(START_REPORT_PATH)
end_accuracy_str = get_accuracy_str(END_REPORT_PATH)

table_rows = "".join([
    f'<tr><td><a href="iter{i}/">iter{i}</a></td><td>{get_accuracy_str(f"reports/iter{i}/results.json")}</td></tr>'
    for i in range(NUM_ITERATIONS)
])

html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Growlibm Report</title>
</head>
<body>
    <h1>GrowLibm Report</h1>
    <p><a href="grow_platform.txt">Grow Platform</a></p>
    
    <h2>Overall Summary</h2>
    <p><strong>Start:</strong> {start_accuracy_str} - <a href="{START_FOLDER}/">Folder</a></p>
    <p><strong>End:</strong> {end_accuracy_str} - <a href="{END_FOLDER}/">Folder</a></p>
    
    <h2>Grow Platform Iterations</h2>
    <table border="1" style="border-collapse:collapse; width:300px;">
        <tr>
            <th>Iteration</th>
            <th>Accuracy</th>
        </tr>
        {table_rows}
    </table>
</body>
</html>
"""

with open(OUTPUT_FILE, 'w') as f:
    f.write(html_content)

print(f"Successfully generated index.html")