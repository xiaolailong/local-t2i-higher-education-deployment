from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

SCRIPT_DIR = Path(__file__).resolve().parent
ROOT_DIR = SCRIPT_DIR.parent / "data" / "benchmark"

OUTPUT_DIR = SCRIPT_DIR.parent / "latex" / "peerj" / "images"
OUTPUT_FILENAME = "figure4.pdf"
SAVE_PNG_PREVIEW = True
PNG_PREVIEW_FILENAME = "figure4.png"

MODEL_HEADERS = ["Qwen-Image-2512", "Z-Image-Turbo", "SDXL-Turbo"]

ROWS = [
    {
        "label": "(a) B7: Object--Concept / Educational",
        "paths": [
            r"subjective\chinese\group_B\c_qwen_B7_chinese_1024-1024_step28_2.png",
            r"subjective\chinese\group_B\c_zimage_B7_chinese_768-768_step9_2.png",
            r"subjective\chinese\group_B\c_sdxl_B7_chinese_768-768_step4_2.png",
        ],
    },
    {
        "label": "(b) B3: Chinese-cultural / Stylized",
        "paths": [
            r"subjective\chinese\group_B\c_qwen_B3_chinese_1024-1024_step28_2.png",
            r"subjective\chinese\group_B\c_zimage_B3_chinese_768-768_step9_2.png",
            r"subjective\chinese\group_B\c_sdxl_B3_chinese_768-768_step4_0.png",
        ],
    },
    {
        "label": "(c) B11: High complexity / Complex Scene",
        "paths": [
            r"subjective\chinese\group_A\c_qwen_B11_chinese_1024-1024_step28_1.png",
            r"subjective\chinese\group_A\c_zimage_B11_chinese_768-768_step9_0.png",
            r"subjective\chinese\group_A\c_sdxl_B11_chinese_768-768_step4_2.png",
        ],
    },
]

# Larger raster canvas for PeerJ figure-file quality; the resulting canvas is
# close to the recommended 3000 px scale while remaining manageable in size.
CELL_SIZE = 900
HEADER_H = 96
ROW_LABEL_H = 105
GAP_X = 22
GAP_Y = 26
MARGIN_X = 24
MARGIN_TOP = 20
MARGIN_BOTTOM = 24
BACKGROUND_COLOR = "white"
TEXT_COLOR = "black"
BORDER_COLOR = (190, 190, 190)


def load_font(size: int = 28, bold: bool = False):
    """Try to load common fonts; use PIL default font if loading fails."""
    font_candidates = []
    if bold:
        font_candidates.extend([
            "DejaVuSans-Bold.ttf",
            "Arial Bold.ttf",
            "C:/Windows/Fonts/arialbd.ttf",
            "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
        ])
    font_candidates.extend([
        "DejaVuSans.ttf",
        "Arial.ttf",
        "C:/Windows/Fonts/arial.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    ])
    for font_path in font_candidates:
        try:
            return ImageFont.truetype(font_path, size=size)
        except Exception:
            continue
    return ImageFont.load_default()


def normalize_rel_path(rel_path: str) -> Path:
    """Support Windows backslashes and Linux/macOS forward slashes."""
    return Path(*rel_path.replace("\\", "/").split("/"))


def resize_to_square(img: Image.Image, size: int) -> Image.Image:
    """Center-crop to a square by aspect ratio, then resize to the target size."""
    w, h = img.size
    side = min(w, h)
    left = (w - side) // 2
    top = (h - side) // 2
    cropped = img.crop((left, top, left + side, top + side))
    return cropped.resize((size, size), Image.LANCZOS)


def draw_centered_text(draw: ImageDraw.ImageDraw, box, text: str, font, fill=TEXT_COLOR):
    """Draw one line of text centered in the given box."""
    x0, y0, x1, y1 = box
    bbox = draw.textbbox((0, 0), text, font=font)
    tw = bbox[2] - bbox[0]
    th = bbox[3] - bbox[1]
    x = x0 + (x1 - x0 - tw) / 2
    y = y0 + (y1 - y0 - th) / 2
    draw.text((x, y), text, fill=fill, font=font)


def main():
    root = ROOT_DIR.expanduser().resolve()
    output_dir = OUTPUT_DIR.expanduser().resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    image_paths = []
    for row in ROWS:
        row_paths = [root / normalize_rel_path(p) for p in row["paths"]]
        image_paths.append(row_paths)

    missing = [str(p) for row_paths in image_paths for p in row_paths if not p.exists()]
    if missing:
        raise FileNotFoundError("The following input images do not exist:\n" + "\n".join(missing))

    header_font = load_font(size=42, bold=True)
    # Row labels describe the task attributes shared by all three images in the row,
    # so they are intentionally larger than before for readability in the manuscript.
    label_font = load_font(size=56, bold=True)

    n_rows = len(ROWS)
    n_cols = len(MODEL_HEADERS)

    canvas_w = MARGIN_X * 2 + n_cols * CELL_SIZE + (n_cols - 1) * GAP_X
    canvas_h = (
        MARGIN_TOP
        + HEADER_H
        + n_rows * CELL_SIZE
        + n_rows * ROW_LABEL_H
        + (n_rows - 1) * GAP_Y
        + MARGIN_BOTTOM
    )
    canvas = Image.new("RGB", (canvas_w, canvas_h), BACKGROUND_COLOR)
    draw = ImageDraw.Draw(canvas)

    y = MARGIN_TOP
    for col, header in enumerate(MODEL_HEADERS):
        x = MARGIN_X + col * (CELL_SIZE + GAP_X)
        draw_centered_text(draw, (x, y, x + CELL_SIZE, y + HEADER_H), header, header_font)

    y = MARGIN_TOP + HEADER_H
    for row_idx, row in enumerate(ROWS):
        for col_idx, img_path in enumerate(image_paths[row_idx]):
            img = Image.open(img_path).convert("RGB")
            img = resize_to_square(img, CELL_SIZE)
            x = MARGIN_X + col_idx * (CELL_SIZE + GAP_X)
            canvas.paste(img, (x, y))
            draw.rectangle((x, y, x + CELL_SIZE - 1, y + CELL_SIZE - 1), outline=BORDER_COLOR, width=1)

        label_y0 = y + CELL_SIZE
        label_y1 = label_y0 + ROW_LABEL_H
        draw_centered_text(draw, (MARGIN_X, label_y0, canvas_w - MARGIN_X, label_y1), row["label"], label_font)
        y = label_y1 + GAP_Y

    output_pdf = output_dir / OUTPUT_FILENAME
    canvas.save(output_pdf, "PDF", resolution=300.0)
    print(f"Saved PDF: {output_pdf}")

    if SAVE_PNG_PREVIEW:
        output_png = output_dir / PNG_PREVIEW_FILENAME
        canvas.save(output_png, dpi=(300, 300))
        print(f"Saved PNG preview: {output_png}")


if __name__ == "__main__":
    main()
