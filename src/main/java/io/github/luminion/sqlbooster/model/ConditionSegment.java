package io.github.luminion.sqlbooster.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.NoSuchElementException;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConditionSegment implements Serializable, Iterable<ConditionSegment> {
    private static final long serialVersionUID = 1L;

    protected LinkedHashSet<Condition> conditions = new LinkedHashSet<>();

    protected boolean and = true;

    protected ConditionSegment next;

    @Override
    public Iterator<ConditionSegment> iterator() {
        return new Itr(this);
    }

    public ConditionSegment appendConditions(Collection<Condition> conditions, boolean isAnd) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        if (isAnd) {
            this.getConditions().addAll(conditions);
        } else {
            // OR 组不能并到当前节点里，否则会把原本的括号结构冲平。
            ConditionSegment conditionSegment = new ConditionSegment();
            conditionSegment.conditions.addAll(conditions);
            conditionSegment.and = false;
            ConditionSegment current = this;
            while (current.getNext() != null) {
                current = current.getNext();
            }
            current.next = conditionSegment;
        }
        return this;
    }

    public ConditionSegment merge(ConditionSegment segment) {
        if (segment == null) {
            return this;
        }
        // 这里按链表节点逐段合并，而不是直接 copy `next`，
        // 这样不同来源的条件片段还能继续复用 appendConditions 的语义。
        for (ConditionSegment node : segment) {
            if (node.getConditions().isEmpty()) {
                continue;
}
            this.appendConditions(node.getConditions(), node.isAnd());
        }
        return this;
    }

    static class Itr implements Iterator<ConditionSegment> {

        private ConditionSegment current;

        public Itr(ConditionSegment root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public ConditionSegment next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            ConditionSegment result = current;
            current = current.getNext();
            return result;
        }
    }

}
