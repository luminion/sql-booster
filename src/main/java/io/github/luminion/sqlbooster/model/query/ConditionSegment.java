package io.github.luminion.sqlbooster.model.query;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.*;

/**
 * 条件组，用于封装一组以 AND 或 OR 连接的查询条件。
 * <p>
 * 通过 {@code next} 字段形成链表，可以构建出复杂的条件树，例如 (A AND B) OR (C AND D)。
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConditionSegment implements Serializable, Iterable<ConditionSegment> {
    private static final long serialVersionUID = 1L;

    protected LinkedHashSet<Condition> conditions = new LinkedHashSet<>();

    protected String connector = SqlKeyword.AND.getSymbol();

    /**
     * 指向下一个条件组，用于构建 OR 连接的子条件。
     */
    protected ConditionSegment next;

    @Override
    public Iterator<ConditionSegment> iterator() {
        return new Itr(this);
    }

    /**
     * 添加一组条件。
     * <p>
     * 如果连接符是 AND，条件会被添加到当前节点；如果是 OR，会创建一个新的子节点。
     *
     * @param conditions 条件集合
     * @param connector  条件间的连接符
     */
    public ConditionSegment appendConditions(Collection<Condition> conditions, String connector) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        SqlKeyword sqlKeyword = SqlKeyword.resolve(connector);
        if (SqlKeyword.OR.equals(sqlKeyword)) {
            // OR 条件，追加到链表末尾
            ConditionSegment node = new ConditionSegment();
            node.conditions.addAll(conditions);
            node.connector = connector;
            ConditionSegment current = this;
            while (current.getNext() != null) {
                current = current.getNext();
            }
            current.next = node;
        } else {
            // AND 条件，合并到当前节点
            this.getConditions().addAll(conditions);
        }
        return this;
    }

    /**
     * 合并另一个条件组。
     *
     * @param segment 要合并的条件组
     */
    public ConditionSegment merge(ConditionSegment segment) {
        if (segment == null) {
            return this;
        }
        for (ConditionSegment node : segment) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            this.appendConditions(node.getConditions(), node.getConnector());
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
