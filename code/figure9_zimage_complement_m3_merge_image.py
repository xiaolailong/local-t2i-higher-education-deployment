from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

SCRIPT_DIR = Path(__file__).resolve().parent
ROOT_DIR = SCRIPT_DIR.parent / "data" / "zimage_complement"

image_filenames = [
    "c_zimage_M3-1_chinese_768-768_step9_2.png",
    "c_zimage_M3-2_chinese_768-768_step9_0.png",
    "c_zimage_M3-3_chinese_768-768_step9_1.png",
]

labels = [
    "(a) Logistics sorting center",
    "(b) Mechanical watch structure",
    "(c) Traditional cultural teaching scene",
]

output_filename = "figure9.pdf"
output_png_filename = "figure9.png"

# Larger raster canvas for PeerJ figure-file quality. The PDF is still generated
# for LaTeX inclusion; the PNG companion is useful if the submission system asks
# for non-vector figure files.
target_h = 900
label_h = 90
gap = 28
background_color = "white"
text_color = "black"


def load_font(size: int = 28):
    """Try to load common fonts; use PIL default font if loading fails."""
    font_candidates = [
        "DejaVuSans.ttf",
        "Arial.ttf",
        "C:/Windows/Fonts/arial.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    ]
    for font_path in font_candidates:
        try:
            return ImageFont.truetype(font_path, size=size)
        except Exception:
            continue
    return ImageFont.load_default()


def main():
    root = ROOT_DIR.expanduser().resolve()

    image_paths = [root / name for name in image_filenames]
    output_path = SCRIPT_DIR.parent / "latex" / "peerj" / "images" / output_filename

    missing = [str(p) for p in image_paths if not p.exists()]
    if missing:
        raise FileNotFoundError("The following input images do not exist:\n" + "\n".join(missing))

    imgs = [Image.open(p).convert("RGB") for p in image_paths]

    resized = []
    for img in imgs:
        w, h = img.size
        new_w = int(w * target_h / h)
        resized.append(img.resize((new_w, target_h), Image.LANCZOS))

    total_w = sum(img.width for img in resized) + gap * (len(resized) - 1)
    canvas = Image.new("RGB", (total_w, target_h + label_h), background_color)

    draw = ImageDraw.Draw(canvas)
    font = load_font(size=42)

    x = 0
    for img, label in zip(resized, labels):
        canvas.paste(img, (x, 0))
        draw.text((x + 12, target_h + 24), label, fill=text_color, font=font)
        x += img.width + gap

    canvas.save(output_path, "PDF", resolution=300.0)

    output_png_path = output_path.with_name(output_png_filename)
    canvas.save(output_png_path, dpi=(300, 300))

    print(f"Saved PDF: {output_path}")
    print(f"Saved PNG: {output_png_path}")


if __name__ == "__main__":
    main()
