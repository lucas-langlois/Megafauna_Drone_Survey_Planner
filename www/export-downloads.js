/* Folder handles remain in this browser tab; no local paths go to the server. */
(() => {
  "use strict";
  let directory = null;
  let busy = false;
  const exports = new Map([
    ["download_complete_plan", ".zip"]
  ]);
  const status = message => {
    document.getElementById("export_folder_status").textContent = message;
  };
  const location = () => directory ? `Folder: ${directory.name}.` :
    "Exports use your browser's download settings.";

  // Never replace an existing export; add a numbered suffix if necessary.
  async function unusedFilename(folder, filename) {
    const dot = filename.lastIndexOf(".");
    const stem = filename.slice(0, dot), extension = filename.slice(dot);
    for (let number = 1; number <= 10000; number++) {
      const candidate = number === 1 ? filename : `${stem}_${number}${extension}`;
      try { await folder.getFileHandle(candidate); }
      catch (error) {
        if (error.name === "NotFoundError") return candidate;
        throw error;
      }
    }
    throw new Error("Too many files with this name. Change the export name.");
  }

  function responseFilename(header, extension) {
    if (!header || !/attachment/i.test(header)) throw new Error("The export was not generated. Check the plan and try again.");
    const utf8 = /filename\*=UTF-8''([^;]+)/i.exec(header);
    const plain = /filename=(?:"([^"]+)"|([^;]+))/i.exec(header);
    let name = utf8 ? decodeURIComponent(utf8[1]) : plain && (plain[1] || plain[2]).trim();
    if (!name || /[\\/\x00-\x1f]/.test(name) || !name.toLowerCase().endsWith(extension)) {
      throw new Error("The server did not return a valid export filename.");
    }
    return name;
  }

  document.addEventListener("DOMContentLoaded", () => {
    const choose = document.getElementById("choose_export_folder");
    const clear = document.getElementById("clear_export_folder");
    if (!window.isSecureContext || typeof window.showDirectoryPicker !== "function") {
      choose.hidden = true;
      clear.hidden = true;
      status("This browser uses standard downloads. To choose a destination, enable 'Ask where to save' in its download settings.");
      return;
    }
    choose.addEventListener("click", async () => {
      if (busy) return;
      try {
        directory = await window.showDirectoryPicker({ mode: "readwrite", id: "megafauna-exports" });
        status(`${location()} Exports will be saved here for this session.`);
      } catch (error) {
        status(error.name === "AbortError" ? location() :
          `Could not select folder: ${error.message} You can still use browser downloads.`);
      }
    });
    clear.addEventListener("click", () => {
      if (busy) return;
      directory = null;
      status(location());
    });
  });

  document.addEventListener("click", async event => {
    const link = event.target.closest("a.shiny-download-link");
    if (!directory || !link || !exports.has(link.id) || link.classList.contains("disabled")) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    if (busy) return;
    busy = true;
    const folder = directory;
    let writable = null;
    try {
      const url = new URL(link.href, window.location.href);
      if (url.origin !== window.location.origin || url.hash || !link.getAttribute("href")) {
        throw new Error("The export is not ready. Check the plan and try again.");
      }
      // Request permission while the export click still has user activation.
      if (await folder.requestPermission({ mode: "readwrite" }) !== "granted") {
        throw new Error("Folder access was not allowed. Choose the folder again or use browser downloads.");
      }
      status("Generating export…");
      const response = await fetch(url.href, { credentials: "same-origin" });
      if (!response.ok) throw new Error("The export could not be generated. Check the plan and try again.");
      const filename = responseFilename(response.headers.get("Content-Disposition"), exports.get(link.id));
      const blob = await response.blob();
      if (!blob.size) throw new Error("The export is empty. Check the plan and try again.");
      const availableName = await unusedFilename(folder, filename);
      const file = await folder.getFileHandle(availableName, { create: true });
      writable = await file.createWritable();
      await writable.write(blob);
      await writable.close();
      writable = null;
      status(`Saved ${availableName} to ${folder.name}.`);
    } catch (error) {
      if (writable) await writable.abort().catch(() => {});
      status(`Export not saved: ${error.message}`);
    } finally {
      busy = false;
    }
  }, true);
})();
