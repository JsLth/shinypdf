import { OCRClient } from "./tesseract-wasm-esm.js";

document.addEventListener("click", async (e) => {
    if (!e.target.closest("#run_ocr")) return;

    const fileInput = document.getElementById("image");
    const file = fileInput?.files?.[0];
    if (!file) return;

    const image = await createImageBitmap(file);
    const ocr = new OCRClient()

    try {
        await ocr.loadModel("./eng.traineddata");
        await ocr.loadImage(image)
        const text = await ocr.getText();
        Shiny.setInputValue("ocr_text", text);
    }
})