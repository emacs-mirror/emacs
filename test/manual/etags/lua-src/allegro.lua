-- ase -- allegro-sprite-editor: the ultimate sprites factory
-- Copyright (C) 2001-2004 by David A. Capello
--
-- Read "LEGAL.txt" for more information.

-- internal routine
local function get_layer_by_name (sprite, layer, name)
  if layer.readable == false then
    return nil;
  end

  if layer.name and strcmp (layer.name, name) == 0 then
    return layer;
  end

  if layer_is_set (layer) then
    local it, sub;

    it = layer.layers;
    while it do
      sub = get_layer_by_name (sprite, it, name)
      if sub then
	return sub;
      end
      it = it.next
    end
  end

  return nil;
end

-- internal routine
local function count_layers (layer)
  local count;

  if layer.parent.type == GFXOBJ_SPRITE then
    count = 0;
  else
    count = 1;
  end

  if layer_is_set (layer) then
    local it = layer.layers;
    while it do
      count = count + count_layers (it);
      it = it.next;
    end
  end

  return count;
end

-- Layer *GetLayerByName (const char *name);
function GetLayerByName (name)
  local sprite = current_sprite;

  if sprite and name then
    return get_layer_by_name (sprite, sprite.set, name);
  else
    return nil;
  end
end

-- const char *GetUniqueLayerName (void);
function GetUniqueLayerName ()
  local sprite = current_sprite;

  if sprite then
    return _("Layer") .. " " .. count_layers (sprite.set);
  else
    return nil;
  end
end

-- void SelectLayer (Layer *layer);
function SelectLayer (layer)
  if current_sprite then
    sprite_set_layer (current_sprite, layer);
  end
end

-- Layer *NewLayer (const char *name, int x, int y, int w, int h);
-- creates a new layer with the "name" in the current sprite (in the
-- current frame) with the specified position and size (if w=h=0 the
-- routine will use the sprite dimension)
function NewLayer (name, x, y, w, h)
  local sprite = current_sprite;
  local layer = nil;
  local image, frame, index;

  if sprite and name then
    if not w or w == 0 then w = sprite.w; end
    if not h or h == 0 then h = sprite.h; end
 
    -- new image
    image = image_new (sprite.imgtype, w, h);
    if not image then
      return nil;
    end

    -- new layer
    layer = layer_image_new (sprite.imgtype, w, h);
    if not layer then
      image_free (image);
      return nil;
    end

    -- clear with mask color
    image_clear (image, 0);

    -- configure layer name and blend mode
    layer_set_name (layer, name);
    layer_set_blend_mode (layer, BLEND_MODE_NORMAL);

    -- add image in the layer stock
    index = stock_add_image (layer.stock, image);

    -- create frame (XXX in the current frpos? --dacap)
    frame = frame_new (sprite.frpos, index, x, y, 255);

    -- add frame
    layer_add_frame (layer, frame);

    -- undo stuff
    if undo_is_enabled (sprite.undo) then
      undo_open (sprite.undo);
      undo_add_layer (sprite.undo, sprite.set, layer);
      undo_set_layer (sprite.undo, sprite);
      undo_close (sprite.undo);
    end

    -- add the layer in the sprite set
    layer_add_layer (sprite.set, layer);

    -- select the new layer
    sprite_set_layer (sprite, layer);
  end

  return layer;
end

-- Layer *NewLayerSet (const char *name);
-- creates a new layer set with the "name" in the current sprite
function NewLayerSet (name)
  local sprite = current_sprite;
  local layer = nil;

  if sprite and name then
    -- new layer
    layer = layer_set_new ();
    if not layer then
      return nil;
    end

    -- configure layer name and blend mode
    layer_set_name (layer, name);

    -- add the layer in the sprite set
    layer_add_layer (sprite.set, layer);

    -- select the new layer
    sprite_set_layer (sprite, layer);
  end

  return layer;
end

-- void RemoveLayer (void);
-- removes the current selected layer
function RemoveLayer ()
  local sprite = current_sprite;

  if sprite and sprite.layer then
    local layer = sprite.layer;
    local parent = layer.parent;
    local layer_select;

    -- select: previous layer, or next layer, or parent (if it is not
    -- the main layer of sprite set)
    if layer.prev then
      layer_select = layer.prev;
    elseif layer.next then
      layer_select = layer.next;
    elseif parent != sprite.set then
      layer_select = parent;
    else
      layer_select = nil;
    end

    -- undo stuff
    if undo_is_enabled (sprite.undo) then
      undo_open (sprite.undo);
      undo_set_layer (sprite.undo, sprite);
      undo_remove_layer (sprite.undo, layer);
      undo_close (sprite.undo);
    end

    -- select other layer
    sprite_set_layer (sprite, layer_select);

    -- remove the layer
    layer_remove_layer (parent, layer);

    -- destroy the layer
    layer_free (layer);
  end
end

-- void MoveLayerTop (void);
-- moves the current layer in the top of the main set
function MoveLayerTop ()
  if current_sprite and current_sprite.layer then
    local layer = current_sprite.layer;

    layer_remove_layer (layer.parent, layer);

    layer_add_layer (current_sprite.set, layer);
  end
end

-- void MoveLayerBottom (void);
-- moves the current layer in the bottom of the main set
function MoveLayerBottom ()
  if current_sprite and current_sprite.layer then
    local layer = current_sprite.layer;

    layer_remove_layer (layer.parent, layer);

    layer_add_layer (current_sprite.set, layer);
    layer_move_layer (current_sprite.set, layer, nil);
  end
end

-- void MoveLayerBefore (Layer *this_one);
-- moves the current layer above the layer "this_one"
function MoveLayerBefore (this_one)
  if current_sprite and current_sprite.layer then
    local layer = current_sprite.layer;
    local layer_dest;

    if not this_one then
      layer_dest = current_sprite.set;
    else
      layer_dest = this_one;
    end

    if layer_dest then
      layer_remove_layer (layer.parent, layer);
      layer_add_layer (layer_dest.parent, layer);
      layer_move_layer (layer_dest.parent, layer, layer_dest);
    end
  end
end

-- void MoveLayerAfter (Layer *this_one);
-- moves the current layer below the layer "this_one" (if that layer
-- is a set, the layer is put in the top of the layer set)
function MoveLayerAfter (this_one)
  if current_sprite and current_sprite.layer then
    local layer = current_sprite.layer;
    local layer_dest;

    if not this_one then
      layer_dest = current_sprite.set;
    else
      layer_dest = this_one;
    end

    if layer_dest then
      layer_remove_layer (layer.parent, layer);

      -- insert in the first position of the set
      if layer_is_set (layer_dest) then
	layer_add_layer (layer_dest, layer);
      -- insert below the layer
      else
	layer_add_layer (layer_dest.parent, layer);
	layer_move_layer (layer_dest.parent, layer, layer_dest.prev);
      end
    end
  end
end
