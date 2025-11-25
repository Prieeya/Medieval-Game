/**
 * Asset Configuration for Medieval Siege Simulator
 * Defines all units with their animations and prompts
 */

const baseStylePrompt = "Top-down 2D orthographic game sprite, realistic medieval style, consistent lighting, consistent shadow direction, transparent background, no perspective, no 3D, slightly stylized, animation frame.";

const assetsConfig = [
  // ============================================================================
  // TOWERS (8 Total)
  // ============================================================================
  {
    category: "towers",
    name: "arrow_tower",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Wooden tower with archer on top, ${baseStylePrompt} idle animation showing archer standing ready` 
      },
      attack: { 
        frames: 5, 
        prompt: `Wooden tower with archer on top, ${baseStylePrompt} attack animation showing archer drawing and firing arrow` 
      },
      death: { 
        frames: 5, 
        prompt: `Wooden tower with archer on top, ${baseStylePrompt} death animation showing tower collapsing and breaking apart` 
      }
    }
  },
  {
    category: "towers",
    name: "catapult_tower",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Stone tower with rotating wooden catapult arm, ${baseStylePrompt} idle animation showing catapult ready` 
      },
      attack: { 
        frames: 6, 
        prompt: `Stone tower with rotating wooden catapult arm, ${baseStylePrompt} attack animation showing catapult loading, winding, and launching rock` 
      },
      death: { 
        frames: 5, 
        prompt: `Stone tower with rotating wooden catapult arm, ${baseStylePrompt} death animation showing stone tower crumbling and catapult breaking` 
      }
    }
  },
  {
    category: "towers",
    name: "crossbow_tower",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Wooden turret with heavy reinforced crossbow, ${baseStylePrompt} idle animation showing crossbow loaded and ready` 
      },
      attack: { 
        frames: 5, 
        prompt: `Wooden turret with heavy reinforced crossbow, ${baseStylePrompt} attack animation showing crossbow firing bolt with precision` 
      },
      death: { 
        frames: 5, 
        prompt: `Wooden turret with heavy reinforced crossbow, ${baseStylePrompt} death animation showing turret breaking apart` 
      }
    }
  },
  {
    category: "towers",
    name: "fire_tower",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Iron brazier tower with flames, ${baseStylePrompt} idle animation showing flames flickering and burning` 
      },
      attack: { 
        frames: 5, 
        prompt: `Iron brazier tower with flames, ${baseStylePrompt} attack animation showing flames intensifying and shooting fireball` 
      },
      death: { 
        frames: 5, 
        prompt: `Iron brazier tower with flames, ${baseStylePrompt} death animation showing flames extinguishing and tower collapsing` 
      }
    }
  },
  {
    category: "towers",
    name: "tesla_tower",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Magical iron spire with glowing blue lightning core, ${baseStylePrompt} idle animation showing blue energy pulsing` 
      },
      attack: { 
        frames: 6, 
        prompt: `Magical iron spire with glowing blue lightning core, ${baseStylePrompt} attack animation showing lightning arcing and chaining to multiple targets` 
      },
      death: { 
        frames: 5, 
        prompt: `Magical iron spire with glowing blue lightning core, ${baseStylePrompt} death animation showing energy fading and spire breaking` 
      }
    }
  },
  {
    category: "towers",
    name: "ballista_tower",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Tall wooden ballista tower with iron bolt, ${baseStylePrompt} idle animation showing ballista loaded and ready` 
      },
      attack: { 
        frames: 6, 
        prompt: `Tall wooden ballista tower with iron bolt, ${baseStylePrompt} attack animation showing ballista firing piercing bolt` 
      },
      death: { 
        frames: 5, 
        prompt: `Tall wooden ballista tower with iron bolt, ${baseStylePrompt} death animation showing ballista breaking and tower collapsing` 
      }
    }
  },
  {
    category: "towers",
    name: "poison_tower",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Tower with bubbling green cauldron, ${baseStylePrompt} idle animation showing green poison bubbling and steaming` 
      },
      attack: { 
        frames: 5, 
        prompt: `Tower with bubbling green cauldron, ${baseStylePrompt} attack animation showing poison cloud being released` 
      },
      death: { 
        frames: 5, 
        prompt: `Tower with bubbling green cauldron, ${baseStylePrompt} death animation showing cauldron breaking and poison spilling` 
      }
    }
  },
  {
    category: "towers",
    name: "bombard_tower",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Large iron cannon tower on stone base, ${baseStylePrompt} idle animation showing cannon ready` 
      },
      attack: { 
        frames: 6, 
        prompt: `Large iron cannon tower on stone base, ${baseStylePrompt} attack animation showing cannon firing explosive shot with smoke` 
      },
      death: { 
        frames: 5, 
        prompt: `Large iron cannon tower on stone base, ${baseStylePrompt} death animation showing cannon exploding and base crumbling` 
      }
    }
  },

  // ============================================================================
  // TRAPS (5 Total)
  // ============================================================================
  {
    category: "traps",
    name: "spike_trap",
    animations: {
      idle: { 
        frames: 2, 
        prompt: `Ground metal spikes hidden, ${baseStylePrompt} idle animation showing spikes concealed in ground` 
      },
      attack: { 
        frames: 4, 
        prompt: `Ground metal spikes emerging, ${baseStylePrompt} attack animation showing spikes popping up from ground` 
      },
      death: { 
        frames: 4, 
        prompt: `Ground metal spikes breaking, ${baseStylePrompt} death animation showing spikes breaking and trap destroyed` 
      }
    }
  },
  {
    category: "traps",
    name: "freeze_trap",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Frost crystal embedded in ground, ${baseStylePrompt} idle animation showing crystal glowing faintly` 
      },
      attack: { 
        frames: 5, 
        prompt: `Frost crystal trap activating, ${baseStylePrompt} attack animation showing ice crystal erupting and freezing area` 
      },
      death: { 
        frames: 4, 
        prompt: `Frost crystal shattering, ${baseStylePrompt} death animation showing crystal breaking apart` 
      }
    }
  },
  {
    category: "traps",
    name: "fire_pit_trap",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Burning crack in ground, ${baseStylePrompt} idle animation showing fire crackling in ground fissure` 
      },
      attack: { 
        frames: 6, 
        prompt: `Burning crack in ground erupting, ${baseStylePrompt} attack animation showing flames bursting upward` 
      },
      death: { 
        frames: 4, 
        prompt: `Fire pit extinguishing, ${baseStylePrompt} death animation showing flames dying out` 
      }
    }
  },
  {
    category: "traps",
    name: "magic_snare_trap",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Purple glowing rune circle on ground, ${baseStylePrompt} idle animation showing runes pulsing with magic` 
      },
      attack: { 
        frames: 5, 
        prompt: `Purple glowing rune circle activating, ${baseStylePrompt} attack animation showing magical roots binding target` 
      },
      death: { 
        frames: 4, 
        prompt: `Magic rune circle fading, ${baseStylePrompt} death animation showing runes fading and circle breaking` 
      }
    }
  },
  {
    category: "traps",
    name: "explosive_barrel",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Wooden barrel with burning fuse, ${baseStylePrompt} idle animation showing barrel with fuse smoldering` 
      },
      attack: { 
        frames: 5, 
        prompt: `Wooden barrel exploding, ${baseStylePrompt} attack animation showing barrel exploding with fire and debris` 
      },
      death: { 
        frames: 3, 
        prompt: `Exploded barrel debris, ${baseStylePrompt} death animation showing barrel destroyed and scattered` 
      }
    }
  },

  // ============================================================================
  // ENEMIES (7 Total)
  // ============================================================================
  {
    category: "enemies",
    name: "grunt_raider",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Leather armor, rusted axe, ${baseStylePrompt} idle animation showing warrior ready` 
      },
      attack: { 
        frames: 5, 
        prompt: `Leather armor, rusted axe, ${baseStylePrompt} attack animation showing warrior swinging axe` 
      },
      death: { 
        frames: 5, 
        prompt: `Leather armor, rusted axe, ${baseStylePrompt} death animation showing warrior falling and dying` 
      },
      move: { 
        frames: 6, 
        prompt: `Leather armor, rusted axe, ${baseStylePrompt} movement animation showing warrior running with axe` 
      }
    }
  },
  {
    category: "enemies",
    name: "brute_crusher",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Huge orc with club, ${baseStylePrompt} idle animation showing massive orc standing ready` 
      },
      attack: { 
        frames: 6, 
        prompt: `Huge orc with club, ${baseStylePrompt} attack animation showing orc swinging massive club` 
      },
      death: { 
        frames: 6, 
        prompt: `Huge orc with club, ${baseStylePrompt} death animation showing orc collapsing heavily` 
      },
      move: { 
        frames: 5, 
        prompt: `Huge orc with club, ${baseStylePrompt} movement animation showing slow heavy orc walking` 
      }
    }
  },
  {
    category: "enemies",
    name: "direwolf",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Dark sprinting wolf, ${baseStylePrompt} idle animation showing wolf crouched and ready` 
      },
      attack: { 
        frames: 4, 
        prompt: `Dark sprinting wolf, ${baseStylePrompt} attack animation showing wolf lunging and biting` 
      },
      death: { 
        frames: 5, 
        prompt: `Dark sprinting wolf, ${baseStylePrompt} death animation showing wolf falling and dying` 
      },
      move: { 
        frames: 8, 
        prompt: `Dark sprinting wolf, ${baseStylePrompt} movement animation showing wolf running fast` 
      }
    }
  },
  {
    category: "enemies",
    name: "shieldbearer",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Armored warrior with large iron shield, ${baseStylePrompt} idle animation showing warrior holding shield` 
      },
      attack: { 
        frames: 5, 
        prompt: `Armored warrior with large iron shield, ${baseStylePrompt} attack animation showing warrior bashing with shield` 
      },
      death: { 
        frames: 5, 
        prompt: `Armored warrior with large iron shield, ${baseStylePrompt} death animation showing warrior falling with shield` 
      },
      move: { 
        frames: 6, 
        prompt: `Armored warrior with large iron shield, ${baseStylePrompt} movement animation showing warrior marching with shield` 
      }
    }
  },
  {
    category: "enemies",
    name: "pyromancer",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Hooded fire mage, ${baseStylePrompt} idle animation showing mage with hands glowing` 
      },
      attack: { 
        frames: 5, 
        prompt: `Hooded fire mage, ${baseStylePrompt} attack animation showing mage casting fireball` 
      },
      death: { 
        frames: 5, 
        prompt: `Hooded fire mage, ${baseStylePrompt} death animation showing mage collapsing` 
      },
      move: { 
        frames: 5, 
        prompt: `Hooded fire mage, ${baseStylePrompt} movement animation showing mage walking with robes flowing` 
      }
    }
  },
  {
    category: "enemies",
    name: "necromancer",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Skull-masked robed caster, ${baseStylePrompt} idle animation showing necromancer with dark aura` 
      },
      attack: { 
        frames: 5, 
        prompt: `Skull-masked robed caster, ${baseStylePrompt} attack animation showing necromancer casting dark magic` 
      },
      death: { 
        frames: 6, 
        prompt: `Skull-masked robed caster, ${baseStylePrompt} death animation showing necromancer collapsing and skeleton minions appearing` 
      },
      move: { 
        frames: 5, 
        prompt: `Skull-masked robed caster, ${baseStylePrompt} movement animation showing necromancer floating forward` 
      }
    }
  },
  {
    category: "enemies",
    name: "skeleton_minion",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Small skeleton with bone dagger, ${baseStylePrompt} idle animation showing skeleton ready` 
      },
      attack: { 
        frames: 4, 
        prompt: `Small skeleton with bone dagger, ${baseStylePrompt} attack animation showing skeleton stabbing` 
      },
      death: { 
        frames: 4, 
        prompt: `Small skeleton with bone dagger, ${baseStylePrompt} death animation showing skeleton collapsing into bones` 
      },
      move: { 
        frames: 6, 
        prompt: `Small skeleton with bone dagger, ${baseStylePrompt} movement animation showing skeleton running` 
      }
    }
  },
  {
    category: "enemies",
    name: "boulder_ram_crew",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Soldiers pushing wooden ram, ${baseStylePrompt} idle animation showing crew with ram ready` 
      },
      attack: { 
        frames: 6, 
        prompt: `Soldiers pushing wooden ram, ${baseStylePrompt} attack animation showing crew ramming with siege weapon` 
      },
      death: { 
        frames: 6, 
        prompt: `Soldiers pushing wooden ram, ${baseStylePrompt} death animation showing crew falling and ram breaking` 
      },
      move: { 
        frames: 5, 
        prompt: `Soldiers pushing wooden ram, ${baseStylePrompt} movement animation showing crew slowly pushing ram forward` 
      }
    }
  },

  // ============================================================================
  // BOSSES (3 Total)
  // ============================================================================
  {
    category: "bosses",
    name: "ironback_minotaur",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Horned beast with iron armor, ${baseStylePrompt} idle animation showing minotaur standing menacingly` 
      },
      attack: { 
        frames: 6, 
        prompt: `Horned beast with iron armor, ${baseStylePrompt} attack animation showing minotaur stomping and charging` 
      },
      death: { 
        frames: 6, 
        prompt: `Horned beast with iron armor, ${baseStylePrompt} death animation showing minotaur collapsing heavily` 
      },
      move: { 
        frames: 6, 
        prompt: `Horned beast with iron armor, ${baseStylePrompt} movement animation showing minotaur walking powerfully` 
      }
    }
  },
  {
    category: "bosses",
    name: "fire_drake",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Wingless lava-skinned drake, ${baseStylePrompt} idle animation showing drake with flames on skin` 
      },
      attack: { 
        frames: 6, 
        prompt: `Wingless lava-skinned drake, ${baseStylePrompt} attack animation showing drake breathing fire breath` 
      },
      death: { 
        frames: 6, 
        prompt: `Wingless lava-skinned drake, ${baseStylePrompt} death animation showing drake collapsing and flames dying` 
      },
      move: { 
        frames: 6, 
        prompt: `Wingless lava-skinned drake, ${baseStylePrompt} movement animation showing drake moving with lava glowing` 
      }
    }
  },
  {
    category: "bosses",
    name: "lich_king_arcthros",
    animations: {
      idle: { 
        frames: 4, 
        prompt: `Tall skeletal sorcerer with frost aura, ${baseStylePrompt} idle animation showing lich king with ice magic swirling` 
      },
      attack: { 
        frames: 6, 
        prompt: `Tall skeletal sorcerer with frost aura, ${baseStylePrompt} attack animation showing lich king casting frost magic and summoning` 
      },
      death: { 
        frames: 6, 
        prompt: `Tall skeletal sorcerer with frost aura, ${baseStylePrompt} death animation showing lich king collapsing and aura fading` 
      },
      move: { 
        frames: 5, 
        prompt: `Tall skeletal sorcerer with frost aura, ${baseStylePrompt} movement animation showing lich king floating forward with frost trail` 
      }
    }
  },

  // ============================================================================
  // ENVIRONMENT
  // ============================================================================
  {
    category: "environment",
    name: "castle_fort",
    animations: {
      idle: { 
        frames: 2, 
        prompt: `Large medieval fort top-down view, ${baseStylePrompt} idle animation showing fort structure` 
      },
      death: { 
        frames: 5, 
        prompt: `Large medieval fort top-down view, ${baseStylePrompt} death animation showing fort collapsing` 
      }
    }
  },
  {
    category: "environment",
    name: "gate",
    animations: {
      idle: { 
        frames: 3, 
        prompt: `Reinforced wooden iron-bound gate, ${baseStylePrompt} idle animation showing gate closed` 
      },
      death: { 
        frames: 5, 
        prompt: `Reinforced wooden iron-bound gate, ${baseStylePrompt} death animation showing gate breaking and falling apart` 
      }
    }
  },
  {
    category: "environment",
    name: "castle_walls",
    animations: {
      idle: { 
        frames: 2, 
        prompt: `Stone walls top-down view, ${baseStylePrompt} idle animation showing wall segment` 
      },
      death: { 
        frames: 4, 
        prompt: `Stone walls top-down view, ${baseStylePrompt} death animation showing wall crumbling` 
      }
    }
  },
  {
    category: "environment",
    name: "castle_corners",
    animations: {
      idle: { 
        frames: 2, 
        prompt: `Turret corners top-down view, ${baseStylePrompt} idle animation showing corner turret` 
      },
      death: { 
        frames: 4, 
        prompt: `Turret corners top-down view, ${baseStylePrompt} death animation showing corner turret collapsing` 
      }
    }
  },
  {
    category: "environment",
    name: "grass_tile",
    animations: {
      idle: { 
        frames: 1, 
        prompt: `Grass tile top-down view, ${baseStylePrompt} idle animation showing medieval battlefield grass` 
      }
    }
  },
  {
    category: "environment",
    name: "path_tile",
    animations: {
      idle: { 
        frames: 1, 
        prompt: `Dirt path tile top-down view, ${baseStylePrompt} idle animation showing worn dirt path` 
      }
    }
  },
  {
    category: "environment",
    name: "background_map",
    animations: {
      idle: { 
        frames: 1, 
        prompt: `Full battlefield top-down view, ${baseStylePrompt} idle animation showing complete medieval battlefield scene` 
      }
    }
  },

  // ============================================================================
  // PROJECTILES
  // ============================================================================
  {
    category: "projectiles",
    name: "arrow",
    animations: {
      move: { 
        frames: 2, 
        prompt: `Arrow projectile flying, ${baseStylePrompt} movement animation showing arrow in flight` 
      }
    }
  },
  {
    category: "projectiles",
    name: "ballista_bolt",
    animations: {
      move: { 
        frames: 2, 
        prompt: `Ballista bolt projectile flying, ${baseStylePrompt} movement animation showing large bolt in flight` 
      }
    }
  },
  {
    category: "projectiles",
    name: "fireball",
    animations: {
      move: { 
        frames: 3, 
        prompt: `Fireball projectile flying, ${baseStylePrompt} movement animation showing fireball with flames trailing` 
      }
    }
  },
  {
    category: "projectiles",
    name: "ice_shard",
    animations: {
      move: { 
        frames: 2, 
        prompt: `Ice shard projectile flying, ${baseStylePrompt} movement animation showing ice crystal in flight` 
      }
    }
  },
  {
    category: "projectiles",
    name: "lightning_bolt",
    animations: {
      move: { 
        frames: 3, 
        prompt: `Lightning bolt projectile, ${baseStylePrompt} movement animation showing electric bolt arcing` 
      }
    }
  },
  {
    category: "projectiles",
    name: "catapult_rock",
    animations: {
      move: { 
        frames: 3, 
        prompt: `Catapult rock projectile flying, ${baseStylePrompt} movement animation showing large rock tumbling through air` 
      }
    }
  }
];

export default assetsConfig;

