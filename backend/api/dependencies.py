"""
Enhanced Dependency Analysis API Endpoints

Provides comprehensive dependency queries including:
- Program callers/callees
- Database object usage
- Copybook dependencies
- Call graph traversal
- Impact analysis
"""
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Dict, Any, Optional, List
import json

from database.db_manager import DatabaseManager

router = APIRouter()


# ==================== Request/Response Models ====================

class ProgramCallersRequest(BaseModel):
    upload_id: str
    program_name: str


class ProgramCalleesRequest(BaseModel):
    upload_id: str
    program_name: str


class DatabaseUsageRequest(BaseModel):
    upload_id: str
    program_name: str


class TableImpactRequest(BaseModel):
    upload_id: str
    table_name: str


class ExecutionPathRequest(BaseModel):
    upload_id: str
    from_program: str
    to_program: str


class CopybookUsageRequest(BaseModel):
    upload_id: str
    copybook_name: str


# ==================== Helper Functions ====================

def get_analysis_run_id(db: DatabaseManager, upload_id: str) -> int:
    """Get analysis_run_id from upload_id (reuses existing db connection)"""
    result = db.execute_query(
        "SELECT id FROM analysis_runs WHERE upload_id = ?",
        (upload_id,)
    )
    if not result:
        raise HTTPException(status_code=404, detail=f"Upload {upload_id} not found")
    return result[0]['id']


# ==================== API Endpoints ====================

@router.get("/dependencies/programs/{upload_id}")
async def get_all_programs(upload_id: str) -> Dict[str, Any]:
    """
    Get all programs in the codebase with summary stats

    Returns:
        List of all programs with call counts, DB usage, etc.
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, upload_id)
        programs = db.execute_query("""
            SELECT
                p.id,
                p.program_name,
                p.program_type,
                p.language,
                p.file_path,
                p.entry_point,
                p.cics_transaction_id,

                -- Count outgoing calls
                (SELECT COUNT(*) FROM program_calls WHERE caller_program_id = p.id) as calls_made,

                -- Count incoming calls
                (SELECT COUNT(*) FROM program_calls WHERE callee_program_id = p.id) as called_by,

                -- Count DB accesses
                (SELECT COUNT(DISTINCT db_object_name) FROM program_db_access WHERE program_id = p.id) as db_objects_used,

                -- Count copybooks used
                (SELECT COUNT(*) FROM program_copybook_usage WHERE program_id = p.id) as copybooks_used

            FROM programs p
            WHERE p.analysis_run_id = ?
            ORDER BY p.program_name
        """, (analysis_run_id,))

        return {
            "upload_id": upload_id,
            "total_programs": len(programs),
            "programs": programs
        }
    finally:
        db.close()


@router.post("/dependencies/callers")
async def get_program_callers(request: ProgramCallersRequest) -> Dict[str, Any]:
    """
    Find all programs that call this program

    Returns:
        List of caller programs with call details
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        callers = db.execute_query("""
            SELECT
                caller.program_name as caller,
                caller.program_type,
                caller.file_path as caller_file,
                pc.call_type,
                pc.call_mechanism,
                pc.caller_line_number,
                pc.parameters_json,
                pc.call_signature
            FROM program_calls pc
            JOIN programs caller ON pc.caller_program_id = caller.id
            JOIN programs callee ON pc.callee_program_id = callee.id
            WHERE callee.program_name = ?
              AND callee.analysis_run_id = ?
            ORDER BY caller.program_name
        """, (request.program_name, analysis_run_id))

        # Parse JSON parameters
        for caller in callers:
            if caller.get('parameters_json'):
                try:
                    caller['parameters'] = json.loads(caller['parameters_json'])
                except:
                    caller['parameters'] = []
            else:
                caller['parameters'] = []

        return {
            "upload_id": request.upload_id,
            "program_name": request.program_name,
            "total_callers": len(callers),
            "callers": callers
        }
    finally:
        db.close()


@router.post("/dependencies/callees")
async def get_program_callees(request: ProgramCalleesRequest) -> Dict[str, Any]:
    """
    Find all programs that this program calls

    Returns:
        List of called programs with call details
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        callees = db.execute_query("""
            SELECT
                pc.callee_program_name as callee,
                callee.program_type,
                callee.file_path as callee_file,
                pc.call_type,
                pc.call_mechanism,
                pc.caller_line_number,
                pc.parameters_json,
                pc.call_signature,
                CASE WHEN pc.callee_program_id IS NULL THEN 1 ELSE 0 END as is_external
            FROM program_calls pc
            JOIN programs caller ON pc.caller_program_id = caller.id
            LEFT JOIN programs callee ON pc.callee_program_id = callee.id
            WHERE caller.program_name = ?
              AND caller.analysis_run_id = ?
            ORDER BY pc.callee_program_name
        """, (request.program_name, analysis_run_id))

        # Parse JSON parameters
        for callee in callees:
            if callee.get('parameters_json'):
                try:
                    callee['parameters'] = json.loads(callee['parameters_json'])
                except:
                    callee['parameters'] = []
            else:
                callee['parameters'] = []

        return {
            "upload_id": request.upload_id,
            "program_name": request.program_name,
            "total_callees": len(callees),
            "callees": callees
        }
    finally:
        db.close()


@router.post("/dependencies/database-usage")
async def get_database_usage(request: DatabaseUsageRequest) -> Dict[str, Any]:
    """
    Find all database objects used by this program

    Returns:
        List of DB tables/procedures with access patterns
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        db_usage = db.execute_query("""
            SELECT
                pda.db_object_name,
                dbo.object_type,
                dbo.database_type,
                pda.access_type,
                pda.access_mode,
                COUNT(*) as access_count,
                GROUP_CONCAT(DISTINCT pda.columns_accessed) as columns,
                GROUP_CONCAT(pda.line_number) as line_numbers
            FROM program_db_access pda
            JOIN programs p ON pda.program_id = p.id
            LEFT JOIN database_objects dbo ON pda.db_object_id = dbo.id
            WHERE p.program_name = ?
              AND p.analysis_run_id = ?
            GROUP BY pda.db_object_name, pda.access_type
            ORDER BY access_count DESC
        """, (request.program_name, analysis_run_id))

        # Parse line numbers
        for usage in db_usage:
            if usage.get('line_numbers'):
                usage['line_numbers'] = [int(ln) for ln in usage['line_numbers'].split(',') if ln]
            else:
                usage['line_numbers'] = []

        return {
            "upload_id": request.upload_id,
            "program_name": request.program_name,
            "total_objects": len(db_usage),
            "database_usage": db_usage
        }
    finally:
        db.close()


@router.post("/dependencies/table-impact")
async def get_table_impact(request: TableImpactRequest) -> Dict[str, Any]:
    """
    Impact analysis: Find all programs affected by changes to this table

    Returns:
        List of programs that access this table and related business rules
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        # Programs that use this table
        programs = db.execute_query("""
            SELECT DISTINCT
                p.program_name,
                p.program_type,
                p.file_path,
                pda.access_type,
                COUNT(*) as access_count,
                GROUP_CONCAT(pda.line_number) as line_numbers
            FROM program_db_access pda
            JOIN programs p ON pda.program_id = p.id
            WHERE pda.db_object_name = ?
              AND p.analysis_run_id = ?
            GROUP BY p.program_name, pda.access_type
            ORDER BY access_count DESC
        """, (request.table_name, analysis_run_id))

        # Parse line numbers
        for prog in programs:
            if prog.get('line_numbers'):
                prog['line_numbers'] = [int(ln) for ln in prog['line_numbers'].split(',') if ln]
            else:
                prog['line_numbers'] = []

        # Business rules related to this table
        business_rules = db.execute_query("""
            SELECT
                br.rule_type,
                br.rule_category,
                br.description,
                p.program_name,
                br.line_number,
                br.code_snippet
            FROM business_rules br
            JOIN database_objects dbo ON br.db_object_id = dbo.id
            LEFT JOIN programs p ON br.program_id = p.id
            WHERE dbo.object_name = ?
              AND dbo.analysis_run_id = ?
        """, (request.table_name, analysis_run_id))

        return {
            "upload_id": request.upload_id,
            "table_name": request.table_name,
            "impacted_programs": {
                "total": len(programs),
                "programs": programs
            },
            "related_business_rules": {
                "total": len(business_rules),
                "rules": business_rules
            }
        }
    finally:
        db.close()


@router.post("/dependencies/execution-path")
async def get_execution_path(request: ExecutionPathRequest) -> Dict[str, Any]:
    """
    Trace execution path from one program to another

    Returns:
        Complete call path with details about each hop
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        # Find path in call_graph_paths
        path_result = db.execute_query("""
            SELECT
                cgp.path_description,
                cgp.path_length,
                cgp.path_json
            FROM call_graph_paths cgp
            JOIN programs source ON cgp.source_program_id = source.id
            JOIN programs target ON cgp.target_program_id = target.id
            WHERE source.program_name = ?
              AND target.program_name = ?
              AND source.analysis_run_id = ?
        """, (request.from_program, request.to_program, analysis_run_id))

        if not path_result:
            return {
                "upload_id": request.upload_id,
                "from_program": request.from_program,
                "to_program": request.to_program,
                "path_found": False,
                "message": f"No call path found from {request.from_program} to {request.to_program}"
            }

        path_data = path_result[0]
        path_ids = json.loads(path_data['path_json'])

        # Enrich with details about each hop
        enriched_path = []
        for i in range(len(path_ids) - 1):
            call_details = db.execute_query("""
                SELECT
                    pc.call_type,
                    pc.call_mechanism,
                    pc.parameters_json,
                    pc.caller_line_number,
                    pc.call_signature,
                    caller.program_name as from_prog,
                    callee.program_name as to_prog
                FROM program_calls pc
                JOIN programs caller ON pc.caller_program_id = caller.id
                JOIN programs callee ON pc.callee_program_id = callee.id
                WHERE pc.caller_program_id = ? AND pc.callee_program_id = ?
                LIMIT 1
            """, (path_ids[i], path_ids[i+1]))

            if call_details:
                detail = call_details[0]
                if detail.get('parameters_json'):
                    try:
                        detail['parameters'] = json.loads(detail['parameters_json'])
                    except:
                        detail['parameters'] = []
                enriched_path.append(detail)

        return {
            "upload_id": request.upload_id,
            "from_program": request.from_program,
            "to_program": request.to_program,
            "path_found": True,
            "path_description": path_data['path_description'],
            "hops": path_data['path_length'],
            "call_details": enriched_path
        }
    finally:
        db.close()


@router.post("/dependencies/copybook-usage")
async def get_copybook_usage(request: CopybookUsageRequest) -> Dict[str, Any]:
    """
    Find all programs that use this copybook

    Returns:
        List of programs using the copybook with context
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        usage = db.execute_query("""
            SELECT
                p.program_name,
                p.program_type,
                p.file_path,
                pcu.usage_context,
                pcu.line_number,
                c.data_structures_json
            FROM program_copybook_usage pcu
            JOIN programs p ON pcu.program_id = p.id
            LEFT JOIN copybooks c ON pcu.copybook_id = c.id
            WHERE pcu.copybook_name = ?
              AND p.analysis_run_id = ?
            ORDER BY p.program_name
        """, (request.copybook_name, analysis_run_id))

        # Parse data structures
        for item in usage:
            if item.get('data_structures_json'):
                try:
                    item['data_structures'] = json.loads(item['data_structures_json'])
                except:
                    item['data_structures'] = []
            else:
                item['data_structures'] = []

        return {
            "upload_id": request.upload_id,
            "copybook_name": request.copybook_name,
            "total_programs": len(usage),
            "usage": usage
        }
    finally:
        db.close()


class ProgramCopybooksRequest(BaseModel):
    upload_id: str
    program_name: str


@router.post("/dependencies/program-copybooks")
async def get_program_copybooks(request: ProgramCopybooksRequest) -> Dict[str, Any]:
    """
    Find all copybooks used by this program

    Returns:
        List of copybooks used by the program
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, request.upload_id)
        copybooks = db.execute_query("""
            SELECT
                c.copybook_name,
                c.copybook_type,
                c.file_path,
                pcu.usage_context,
                pcu.line_number,
                c.data_structures_json
            FROM program_copybook_usage pcu
            JOIN programs p ON pcu.program_id = p.id
            LEFT JOIN copybooks c ON pcu.copybook_id = c.id
            WHERE p.program_name = ?
              AND p.analysis_run_id = ?
            ORDER BY c.copybook_name
        """, (request.program_name, analysis_run_id))

        # Parse data structures
        for item in copybooks:
            if item.get('data_structures_json'):
                try:
                    item['data_structures'] = json.loads(item['data_structures_json'])
                except:
                    item['data_structures'] = []
            else:
                item['data_structures'] = []

        return {
            "upload_id": request.upload_id,
            "program_name": request.program_name,
            "total_copybooks": len(copybooks),
            "copybooks": copybooks
        }
    finally:
        db.close()


@router.get("/dependencies/summary/{upload_id}")
async def get_dependency_summary(upload_id: str) -> Dict[str, Any]:
    """
    Get overall dependency summary for the codebase

    Returns:
        Summary statistics about programs, DB objects, calls, etc.
    """
    db = DatabaseManager()
    try:
        analysis_run_id = get_analysis_run_id(db, upload_id)
        summary = {}

        # Program counts
        prog_counts = db.execute_query("""
            SELECT
                COUNT(*) as total_programs,
                SUM(CASE WHEN entry_point = 1 THEN 1 ELSE 0 END) as entry_points,
                COUNT(DISTINCT program_type) as program_types
            FROM programs
            WHERE analysis_run_id = ?
        """, (analysis_run_id,))
        summary['programs'] = prog_counts[0] if prog_counts else {}

        # DB object counts
        db_counts = db.execute_query("""
            SELECT
                COUNT(*) as total_objects,
                COUNT(DISTINCT object_type) as object_types,
                SUM(CASE WHEN object_type = 'TABLE' THEN 1 ELSE 0 END) as tables,
                SUM(CASE WHEN object_type = 'PROCEDURE' THEN 1 ELSE 0 END) as procedures
            FROM database_objects
            WHERE analysis_run_id = ?
        """, (analysis_run_id,))
        summary['database_objects'] = db_counts[0] if db_counts else {}

        # Call counts
        call_counts = db.execute_query("""
            SELECT
                COUNT(*) as total_calls,
                COUNT(DISTINCT call_type) as call_types,
                SUM(CASE WHEN callee_program_id IS NULL THEN 1 ELSE 0 END) as external_calls
            FROM program_calls
            WHERE analysis_run_id = ?
        """, (analysis_run_id,))
        summary['program_calls'] = call_counts[0] if call_counts else {}

        # Copybook counts
        copy_counts = db.execute_query("""
            SELECT
                COUNT(DISTINCT copybook_name) as total_copybooks,
                COUNT(*) as total_usages
            FROM program_copybook_usage
            WHERE analysis_run_id = ?
        """, (analysis_run_id,))
        summary['copybooks'] = copy_counts[0] if copy_counts else {}

        # Most called programs (top 10)
        most_called = db.execute_query("""
            SELECT
                p.program_name,
                COUNT(*) as call_count
            FROM program_calls pc
            JOIN programs p ON pc.callee_program_id = p.id
            WHERE p.analysis_run_id = ?
            GROUP BY p.program_name
            ORDER BY call_count DESC
            LIMIT 10
        """, (analysis_run_id,))
        summary['most_called_programs'] = most_called

        # Most used tables (top 10)
        most_used_tables = db.execute_query("""
            SELECT
                db_object_name,
                COUNT(DISTINCT program_id) as program_count,
                COUNT(*) as access_count
            FROM program_db_access
            WHERE analysis_run_id = ?
            GROUP BY db_object_name
            ORDER BY program_count DESC
            LIMIT 10
        """, (analysis_run_id,))
        summary['most_used_tables'] = most_used_tables

        return {
            "upload_id": upload_id,
            "summary": summary
        }
    finally:
        db.close()
