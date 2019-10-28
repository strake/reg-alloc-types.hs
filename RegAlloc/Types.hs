-- This module is based on the "linearscan" package by John Wiegley.
-- It is meant to present a similar interface.
-- http://github.com/jwiegley/linearscan
module RegAlloc.Types where

-- | Each "virtual variable" has details associated with it that affect the
-- allocation procedure.
data VarInfo = VarInfo
    { varId       :: Either PhysReg VarId
      -- ^ Identify the variable, or if it is an explicit register reference.

    , varKind     :: VarKind
      -- ^ The kind of a variable determines the scope of its lifetime, and
      -- when it is spilled or loaded to or from stack. For example, output
      -- variables are not needed in a basic block until the first point of
      -- use, while the lifetime of input variables extends until their final
      -- use.

    , regRequired :: Bool
      -- ^ If true, the variable's value must be loaded into a register at
      -- this use position.
    }
  deriving (Eq, Show)

type VarId = Int
data VarKind = Input | InputOutput | Temp | Output deriving (Eq, Show)

-- | Every operation may reference multiple variables and/or specific physical
--   registers.  If a physical register is referenced, then that register is
--   considered unavailable for allocation over its range of use.
--
--   Certain operations have special significance as to how basic blocks are
--   organized and lifetime of allocations. Thus, if an operation begins or
--   ends a loop, or represents a method call, this should be indicated using
--   the 'OpKind' field. Indication of calls is necessary for saving and
--   restoring all registers around a call, while indication of loops is
--   optional, as it merely avoids reloading spilled variables inside loop
--   bodies.
data OpInfo m op1 op2 = OpInfo
    { opKind      :: op1 -> OpKind
      -- ^ Return the kind of operator prior to allocation.

    , opRefs      :: op1 -> [VarInfo]
      -- ^ Return all variable references for the operation.

    , moveOp      :: PhysReg -> VarId -> PhysReg -> m [op2]
      -- ^ Create move instruction(s) from one register to another, relating
      -- to the given variable.

    , saveOp      :: PhysReg -> VarId -> m [op2]
      -- ^ Create a spill instruction from the given restriction, to a stack
      -- slot for the given variable.

    , restoreOp   :: VarId -> PhysReg -> m [op2]
      -- ^ Create a load instruction from the stack slot for the given
      -- variable, to the given register.

    , applyAllocs :: op1 -> [((VarId, VarKind), PhysReg)] -> m [op2]
      -- ^ Given an operation, and a set of register allocations for each
      -- variable used by the operation (differentiated by type of use), apply
      -- the allocations to create one or more post-allocation operations.

    , showOp1     :: op1 -> String
      -- ^ Render the given pre-allocation operation as a string.
    }
  deriving (Functor)

data OpKind = IsNormal | IsCall | IsBranch deriving (Eq, Show)

-- | From the point of view of this library, a basic block is nothing more
--   than an ordered sequence of operations.
data BlockInfo m blk1 blk2 op1 op2 = BlockInfo
    { blockId           :: blk1 -> Int
      -- ^ Identify the block with a unique number. The nature and ordering of
      -- the number is not significant, only its uniqueness.

    , blockSuccessors   :: blk1 -> [Int]
      -- ^ The immediate successors of a block.

    , splitCriticalEdge :: blk1 -> blk1 -> m (blk1, blk1)
      -- ^ Given two blocks, insert a new block between them to break up a
      -- "critical edge" (where the first block has multiple destinations due
      -- to a conditional branch, for example, while the second block has
      -- multiple originations due to branches from other blocks). The result
      -- is the new pair of blocks at that boundary. Typically, only one of
      -- the two will be a newly created block.

    , blockOps          :: blk1 -> ([op1], [op1], [op1])
      -- ^ Return the entry, body, and exit operation of a block. Typically,
      -- the entry operation is a "label", and the exit operation is a branch,
      -- jump or return.

    , setBlockOps       :: blk1 -> [op2] -> [op2] -> [op2] -> blk2
      -- ^ Replace the set of operations for a block with a set of allocated
      -- operations.
    }

type PhysReg = Int

data UseVerifier = VerifyDisabled | VerifyEnabled | VerifyEnabledStrict
  deriving (Eq, Show)
