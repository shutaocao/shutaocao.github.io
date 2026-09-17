# Ruby 3.2+ removed the taint/trust APIs entirely. The old Liquid version
# pinned by the `github-pages` gem (still used to match GitHub's build
# environment) calls String#tainted? while rendering, which crashes local
# `jekyll serve`/`build` with `undefined method 'tainted?'`. Taint checking
# was already an inert no-op by the time Ruby removed it, so restoring these
# as harmless stubs is safe and only affects local rendering.
unless Object.method_defined?(:tainted?)
  class Object
    def tainted?
      false
    end

    def untaint
      self
    end

    def untrusted?
      false
    end

    def untrust
      self
    end

    def trust
      self
    end
  end
end

# Pathutil 0.16.2 (a jekyll dependency) forwards its keyword-arg hash to
# File.read/binread positionally (`File.read(self, *args, kwd)`), which
# Ruby 3+ no longer auto-converts to keyword arguments. That raises
# "no implicit conversion of Hash into Integer" the first time Jekyll's
# `serve --watch` probes /proc/version for WSL detection. Reopen the two
# methods with the fixed `**kwd` splat.
if defined?(Pathutil)
  class Pathutil
    def read(*args, **kwd)
      kwd[:encoding] ||= encoding

      if normalize[:read]
        File.read(self, *args, **kwd).encode({
          :universal_newline => true,
        })
      else
        File.read(self, *args, **kwd)
      end
    end

    def binread(*args, **kwd)
      kwd[:encoding] ||= encoding

      if normalize[:read]
        File.binread(self, *args, **kwd).encode({
          :universal_newline => true,
        })
      else
        File.binread(self, *args, **kwd)
      end
    end
  end
end
