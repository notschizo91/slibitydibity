// three.js glue for the 3D preview. All geometry math lives on the F# side;
// this module manages the scene, camera, materials, and the keyring drag.
import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';

export function createViewer(container) {
  const scene = new THREE.Scene();

  const camera = new THREE.PerspectiveCamera(45, 1, 0.1, 100000);
  camera.up.set(0, 0, 1); // Z-up, matching STL convention
  camera.position.set(0, -110, 130);

  const renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  container.appendChild(renderer.domElement);

  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;

  scene.add(new THREE.AmbientLight(0xffffff, 0.55));
  const key = new THREE.DirectionalLight(0xffffff, 1.6);
  key.position.set(1, -1.2, 2.2);
  scene.add(key);
  const fill = new THREE.DirectionalLight(0x8b5cf6, 0.5);
  fill.position.set(-1.5, 1, 0.6);
  scene.add(fill);

  const grid = new THREE.GridHelper(600, 60, 0x8b5cf6, 0x231b38);
  grid.rotation.x = Math.PI / 2; // into the XY plane (Z-up world)
  grid.material.transparent = true;
  grid.material.opacity = 0.4;
  scene.add(grid);

  const viewer = { scene, camera, renderer, controls, meshes: new Map(), container };

  const resize = () => {
    const w = container.clientWidth;
    const h = container.clientHeight;
    if (w === 0 || h === 0) return;
    camera.aspect = w / h;
    camera.updateProjectionMatrix();
    renderer.setSize(w, h);
  };
  new ResizeObserver(resize).observe(container);
  resize();

  renderer.setAnimationLoop(() => {
    controls.update();
    renderer.render(scene, camera);
  });

  return viewer;
}

export function setMesh(viewer, id, positions, color) {
  const geo = new THREE.BufferGeometry();
  geo.setAttribute('position', new THREE.BufferAttribute(new Float32Array(positions), 3));
  geo.computeVertexNormals(); // non-indexed -> per-face normals, flat STL look
  let mesh = viewer.meshes.get(id);
  if (mesh) {
    mesh.geometry.dispose();
    mesh.geometry = geo;
    mesh.material.color.set(color);
  } else {
    const mat = new THREE.MeshStandardMaterial({ color, roughness: 0.55, metalness: 0.1 });
    mesh = new THREE.Mesh(geo, mat);
    viewer.meshes.set(id, mesh);
    viewer.scene.add(mesh);
  }
}

export function setColor(viewer, id, color) {
  const mesh = viewer.meshes.get(id);
  if (mesh) mesh.material.color.set(color);
}

export function removeMesh(viewer, id) {
  const mesh = viewer.meshes.get(id);
  if (!mesh) return;
  viewer.scene.remove(mesh);
  mesh.geometry.dispose();
  mesh.material.dispose();
  viewer.meshes.delete(id);
}

export function clearMeshes(viewer) {
  for (const id of [...viewer.meshes.keys()]) removeMesh(viewer, id);
}

export function fitView(viewer) {
  if (viewer.meshes.size === 0) return;
  const box = new THREE.Box3();
  for (const mesh of viewer.meshes.values()) box.expandByObject(mesh);
  const center = box.getCenter(new THREE.Vector3());
  const sphere = box.getBoundingSphere(new THREE.Sphere());
  const radius = Math.max(sphere.radius, 1);
  const dist = (radius / Math.tan((viewer.camera.fov * Math.PI) / 360)) * 1.3;
  const dir = viewer.camera.position.clone().sub(viewer.controls.target);
  if (dir.lengthSq() < 1e-6) dir.set(0, -0.7, 1);
  dir.normalize();
  viewer.camera.position.copy(center.clone().add(dir.multiplyScalar(dist)));
  viewer.camera.near = Math.max(dist / 1000, 0.01);
  viewer.camera.far = dist * 100;
  viewer.camera.updateProjectionMatrix();
  viewer.controls.target.copy(center);
  viewer.controls.update();
}

/**
 * Register a draggable target on the z=0 plane. getZone() returns
 * {X, Y, R} (centre + grab radius, world mm) or null when inactive;
 * onMove({x, y}) fires while dragging. Multiple targets share one set of
 * pointer listeners; the closest zone (normalized by radius) wins.
 */
export function registerDrag(viewer, getZone, onMove) {
  if (!viewer.dragTargets) {
    viewer.dragTargets = [];
    const el = viewer.renderer.domElement;
    const ray = new THREE.Raycaster();
    const ndc = new THREE.Vector2();
    let active = null;

    const toPlane = (ev) => {
      const rect = el.getBoundingClientRect();
      ndc.x = ((ev.clientX - rect.left) / rect.width) * 2 - 1;
      ndc.y = -((ev.clientY - rect.top) / rect.height) * 2 + 1;
      ray.setFromCamera(ndc, viewer.camera);
      const dz = ray.ray.direction.z;
      if (Math.abs(dz) < 1e-9) return null;
      const t = -ray.ray.origin.z / dz;
      if (t < 0) return null;
      return {
        x: ray.ray.origin.x + t * ray.ray.direction.x,
        y: ray.ray.origin.y + t * ray.ray.direction.y,
      };
    };

    const pick = (p) => {
      if (!p) return null;
      let best = null;
      let bestScore = Infinity;
      for (const target of viewer.dragTargets) {
        const z = target.getZone();
        if (!z) continue;
        const d = Math.hypot(p.x - z.X, p.y - z.Y);
        const score = d / z.R;
        if (d <= z.R * 1.2 && score < bestScore) {
          bestScore = score;
          best = target;
        }
      }
      return best;
    };

    el.addEventListener('pointerdown', (ev) => {
      const target = pick(toPlane(ev));
      if (target) {
        active = target;
        viewer.controls.enabled = false;
        el.setPointerCapture(ev.pointerId);
      }
    });
    el.addEventListener('pointermove', (ev) => {
      const p = toPlane(ev);
      if (active) {
        if (p) active.onMove(p);
      } else {
        el.style.cursor = pick(p) ? 'grab' : '';
      }
    });
    const stop = (ev) => {
      if (active) {
        active = null;
        viewer.controls.enabled = true;
        try {
          el.releasePointerCapture(ev.pointerId);
        } catch {}
      }
    };
    el.addEventListener('pointerup', stop);
    el.addEventListener('pointercancel', stop);
  }
  viewer.dragTargets.push({ getZone, onMove });
}
