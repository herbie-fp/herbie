#!/usr/bin/env python3

import json
import sys
from datetime import datetime
from pathlib import Path

command = sys.argv[1]
path = Path(sys.argv[2])
now = datetime.now().astimezone()
timestamp = now.isoformat(timespec="microseconds")

if command == "init":
    checkpoints = [{"phase": "start", "timestamp": timestamp, "elapsed_seconds": 0}]
else:
    checkpoints = json.loads(path.read_text(encoding="utf-8"))
    start = datetime.fromisoformat(checkpoints[0]["timestamp"])
    checkpoints.append(
        {
            "phase": sys.argv[3],
            "timestamp": timestamp,
            "elapsed_seconds": round((now - start).total_seconds(), 6),
        }
    )

path.parent.mkdir(parents=True, exist_ok=True)
path.write_text(json.dumps(checkpoints, indent=2) + "\n", encoding="utf-8")
