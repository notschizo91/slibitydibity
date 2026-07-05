// File System Access API glue for the "Save Folder" workflow (Chrome/Edge).
// Falls back to normal downloads when unsupported — callers check canPickFolder.

export function canPickFolder() {
  return 'showDirectoryPicker' in window;
}

export async function pickFolder() {
  try {
    return await window.showDirectoryPicker({ mode: 'readwrite' });
  } catch {
    return null; // user cancelled
  }
}

export async function saveToFolder(handle, filename, buffer) {
  const file = await handle.getFileHandle(filename, { create: true });
  const writable = await file.createWritable();
  await writable.write(buffer);
  await writable.close();
  return true;
}
