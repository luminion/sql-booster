package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

/**
 * SQL 条件树实体类.
 * <p>
 * 实现了 {@link QuerySegment} 接口, 用于表示 SQL 查询条件的树形结构, 支持嵌套和复杂的查询逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
public class QuerySegment implements Iterable<QuerySegment> {
    /**
     * 当前节点的条件列表.
     */
    protected LinkedHashSet<Condition> conditions = new LinkedHashSet<>();
    /**
     * 连接当前节点条件的逻辑连接符.
     */
    protected String connector = SqlKeyword.AND.getKeyword();
    /**
     * 子条件树.
     */
    protected QuerySegment next;

    @Override
    public Iterator<QuerySegment> iterator() {
        return new Itr(this);
    }

    public QuerySegment appendConditions(Collection<Condition> conditions, String connector) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        connector = SqlKeyword.replaceConnector(connector);
        if (SqlKeyword.OR.getKeyword().equals(connector)) {
            // append or conditions to last node
            QuerySegment node = new QuerySegment();
            node.conditions.addAll(conditions);
            node.connector = connector;
            QuerySegment current = this;
            while (current.getNext() != null) {
                current = current.getNext();
            }
            current.next = node;
        } else {
            // put and conditions to current level
            this.getConditions().addAll(conditions);
        }
        return this;
    }

    protected QuerySegment appendTree(QuerySegment segment) {
        if (segment == null) {
            return this;
        }
        for (QuerySegment node : segment) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            this.appendConditions(node.getConditions(), node.getConnector());
        }
        return this;
    }

    static class Itr implements Iterator<QuerySegment> {

        private QuerySegment current;

        public Itr(QuerySegment root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public QuerySegment next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            QuerySegment result = current;
            current = current.getNext();
            return result;
        }
    }

}