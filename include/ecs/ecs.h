#ifndef ECS_H
#define ECS_H

#include <assert.h>
#include <common.h>
#include <klib/khash.h>
#include <lisp/object.h>
#include <stdint.h>

/* Entity ID → newest generation. */
KHASH_MAP_INIT_INT(gen, u16);
/* Entity IDs of live entities. */
KHASH_SET_INIT_INT(live);

struct World;
struct Archetype;

#define ENT_ID_OFFSET (32 - OBJ_TAG_LENGTH)
#define ENT_GEN_OFFSET (16 - OBJ_TAG_LENGTH)
#define ENT_ID(ENTITY) (OBJ_UNBOX(ENTITY) >> ENT_ID_OFFSET)
#define ENT_GEN(ENTITY) ((u16)(0xFFFF & (OBJ_UNBOX(ENTITY) >> ENT_GEN_OFFSET)))
#define ENT_BOX(ID, GEN)                                                       \
  OBJ_BOX((((ID) << ENT_ID_OFFSET) & ((GEN) << ENT_GEN_OFFSET)), ENTITY)


struct World* init_world();
Object new_entity(struct World *world);
Object new_component(struct World *world);
bool entity_live(struct World *world, Object entity);

#endif
