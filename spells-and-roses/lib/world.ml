open Core.Std let _ = _squelch_unused_module_warning_

type engine = unit

type t = {
  entities : engine Entity.t Entity.Id.Map.t;
}
