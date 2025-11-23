import React, { useState, useEffect } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  Alert,
  CircularProgress,
  Grid,
  List,
  ListItem,
  ListItemText,
  Chip,
  Divider,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
} from '@mui/material';
import {
  Code as ProgramIcon,
  Rule as RuleIcon,
  Warning as WarningIcon,
} from '@mui/icons-material';
import api from '../../services/api';

interface DatabaseImpactViewProps {
  uploadId: string;
  tableName: string;
}

const DatabaseImpactView: React.FC<DatabaseImpactViewProps> = ({
  uploadId,
  tableName,
}) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [impactData, setImpactData] = useState<any>(null);

  useEffect(() => {
    loadImpactAnalysis();
  }, [uploadId, tableName]);

  const loadImpactAnalysis = async () => {
    setLoading(true);
    setError(null);

    try {
      const data = await api.getTableImpact(uploadId, tableName);
      setImpactData(data);
      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load impact analysis');
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" p={4}>
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ mb: 2 }}>
        {error}
      </Alert>
    );
  }

  if (!impactData) {
    return null;
  }

  const programs = impactData.impacted_programs?.programs || [];
  const rules = impactData.related_business_rules?.rules || [];

  const readPrograms = programs.filter((p: any) => p.access_type === 'SELECT');
  const writePrograms = programs.filter((p: any) =>
    ['INSERT', 'UPDATE', 'DELETE'].includes(p.access_type)
  );

  return (
    <Grid container spacing={3}>
      {/* Summary Card */}
      <Grid item xs={12}>
        <Alert severity="warning" icon={<WarningIcon />}>
          <Typography variant="body1" gutterBottom>
            <strong>Impact Analysis for: {tableName}</strong>
          </Typography>
          <Typography variant="body2">
            This table is used by <strong>{programs.length} programs</strong> and has{' '}
            <strong>{rules.length} business rules</strong> associated with it.
          </Typography>
        </Alert>
      </Grid>

      {/* Programs Summary */}
      <Grid item xs={12} md={6}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              <ProgramIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
              Impacted Programs
            </Typography>
            <Divider sx={{ mb: 2 }} />
            <Box display="flex" gap={2} mb={2}>
              <Box>
                <Typography variant="h4" color="info.main">
                  {readPrograms.length}
                </Typography>
                <Typography variant="caption" color="textSecondary">
                  Read Access
                </Typography>
              </Box>
              <Box>
                <Typography variant="h4" color="error.main">
                  {writePrograms.length}
                </Typography>
                <Typography variant="caption" color="textSecondary">
                  Write Access
                </Typography>
              </Box>
            </Box>
          </CardContent>
        </Card>
      </Grid>

      {/* Business Rules Summary */}
      <Grid item xs={12} md={6}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              <RuleIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
              Business Rules
            </Typography>
            <Divider sx={{ mb: 2 }} />
            <Typography variant="h4" color="primary">
              {rules.length}
            </Typography>
            <Typography variant="caption" color="textSecondary">
              Rules to Review
            </Typography>
          </CardContent>
        </Card>
      </Grid>

      {/* Programs Table */}
      <Grid item xs={12}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              Programs Using This Table
            </Typography>
            <Divider sx={{ mb: 2 }} />
            {programs.length === 0 ? (
              <Typography variant="body2" color="textSecondary">
                No programs found using this table
              </Typography>
            ) : (
              <TableContainer component={Paper} variant="outlined">
                <Table size="small">
                  <TableHead>
                    <TableRow>
                      <TableCell>Program Name</TableCell>
                      <TableCell>Type</TableCell>
                      <TableCell>Access Type</TableCell>
                      <TableCell align="right">Access Count</TableCell>
                      <TableCell>Lines</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {programs.map((program: any, idx: number) => (
                      <TableRow key={idx}>
                        <TableCell>
                          <Typography variant="body2" fontWeight="bold">
                            {program.program_name}
                          </Typography>
                          <Typography variant="caption" color="textSecondary">
                            {program.file_path}
                          </Typography>
                        </TableCell>
                        <TableCell>
                          <Chip label={program.program_type} size="small" />
                        </TableCell>
                        <TableCell>
                          <Chip
                            label={program.access_type}
                            size="small"
                            color={
                              program.access_type === 'SELECT'
                                ? 'info'
                                : 'error'
                            }
                          />
                        </TableCell>
                        <TableCell align="right">
                          {program.access_count}
                        </TableCell>
                        <TableCell>
                          <Typography variant="caption">
                            {program.line_numbers?.slice(0, 5).join(', ')}
                            {program.line_numbers?.length > 5 && '...'}
                          </Typography>
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </TableContainer>
            )}
          </CardContent>
        </Card>
      </Grid>

      {/* Business Rules List */}
      {rules.length > 0 && (
        <Grid item xs={12}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                Related Business Rules
              </Typography>
              <Divider sx={{ mb: 2 }} />
              <List>
                {rules.map((rule: any, idx: number) => (
                  <React.Fragment key={idx}>
                    <ListItem alignItems="flex-start">
                      <ListItemText
                        primary={
                          <Box display="flex" alignItems="center" gap={1}>
                            <Typography variant="subtitle2">
                              {rule.rule_type}
                            </Typography>
                            {rule.rule_category && (
                              <Chip
                                label={rule.rule_category}
                                size="small"
                                variant="outlined"
                              />
                            )}
                          </Box>
                        }
                        secondary={
                          <>
                            <Typography variant="body2" color="textPrimary">
                              {rule.description}
                            </Typography>
                            {rule.program_name && (
                              <Typography variant="caption" color="textSecondary">
                                Program: {rule.program_name} (Line {rule.line_number})
                              </Typography>
                            )}
                            {rule.code_snippet && (
                              <Typography
                                variant="caption"
                                component="pre"
                                sx={{
                                  mt: 1,
                                  p: 1,
                                  bgcolor: 'grey.100',
                                  borderRadius: 1,
                                  overflow: 'auto',
                                }}
                              >
                                {rule.code_snippet}
                              </Typography>
                            )}
                          </>
                        }
                      />
                    </ListItem>
                    {idx < rules.length - 1 && <Divider />}
                  </React.Fragment>
                ))}
              </List>
            </CardContent>
          </Card>
        </Grid>
      )}
    </Grid>
  );
};

export default DatabaseImpactView;
