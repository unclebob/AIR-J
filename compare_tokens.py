#!/usr/bin/env python3

import argparse
import re
import sys
from pathlib import Path


TOKEN_PATTERN = re.compile(r'''"(?:\\.|[^"\\])*"|[()\[\]{}]|[^\s()\[\]{}]+''')


def count_tokens(text: str) -> int:
    return len(TOKEN_PATTERN.findall(text))


def read_stats(path: Path) -> dict[str, float]:
    text = path.read_text()
    return {
        "chars": len(text),
        "lines": text.count("\n") + (0 if text.endswith("\n") else 1),
        "tokens": count_tokens(text),
    }


def format_ratio(left: float, right: float) -> str:
    if right == 0:
        return "n/a"
    return f"{left / right:.2f}x"


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare lexical token counts between two source files."
    )
    parser.add_argument("left", help="Path to the first file")
    parser.add_argument("right", help="Path to the second file")
    args = parser.parse_args()

    left_path = Path(args.left)
    right_path = Path(args.right)

    missing = [str(path) for path in (left_path, right_path) if not path.exists()]
    if missing:
        for path in missing:
            print(f"missing file: {path}", file=sys.stderr)
        return 1

    left = read_stats(left_path)
    right = read_stats(right_path)

    left_label = str(left_path)
    right_label = str(right_path)
    width = max(len(left_label), len(right_label), len("metric"))

    print(f"{'metric':<{width}}  {left_label:>12}  {right_label:>12}")
    print(f"{'chars':<{width}}  {left['chars']:>12.0f}  {right['chars']:>12.0f}")
    print(f"{'lines':<{width}}  {left['lines']:>12.0f}  {right['lines']:>12.0f}")
    print(f"{'lex_tokens':<{width}}  {left['tokens']:>12.0f}  {right['tokens']:>12.0f}")
    print()
    print(
        f"ratio {left_label} / {right_label}: "
        f"chars={format_ratio(left['chars'], right['chars'])}, "
        f"lines={format_ratio(left['lines'], right['lines'])}, "
        f"lex_tokens={format_ratio(left['tokens'], right['tokens'])}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
